# syntax=docker/dockerfile:1

# ---- Build stage: compile with sbt 2 and stage all runtime jars ----
FROM eclipse-temurin:21-jdk-noble@sha256:bd6ac06e2a2df928af21c75c4a8a0f2829a93377a65761d6a577bb32281e2ac3 AS build
ARG SBT_VERSION=2.0.11
ARG SBT_SHA256=d8409a244e97c3839f27c4f435f49421196c126a450d30f4cf53522d39e3c379
RUN curl -fsSL -o /tmp/sbt.tgz "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" \
    && echo "${SBT_SHA256}  /tmp/sbt.tgz" | sha256sum -c - \
    && tar -xzf /tmp/sbt.tgz -C /opt && rm /tmp/sbt.tgz
ENV PATH="/opt/sbt/bin:${PATH}"
WORKDIR /src

# Resolve dependencies first so this layer is cached across source changes.
COPY build.sbt ./
COPY project/build.properties project/plugins.sbt project/
RUN --mount=type=cache,target=/root/.cache \
    --mount=type=cache,target=/root/.sbt \
    sbt -batch root/update

COPY src src
RUN --mount=type=cache,target=/root/.cache \
    --mount=type=cache,target=/root/.sbt \
    sbt -batch stage

# ---- Runtime stage: slim JRE 21, non-root ----
FROM eclipse-temurin:21-jre-noble@sha256:161310cd5bd41fd549d19cf9f90b614897cb4a1532555c613b43b7e0628fe5d6
# curl is used by the healthcheck; installed explicitly instead of relying on the base image.
RUN apt-get update \
    && apt-get install -y --no-install-recommends curl \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --system --gid 10001 app \
    && useradd --system --uid 10001 --gid app --home-dir /app app \
    && mkdir -p /app/lib /data && chown app:app /data
WORKDIR /app
COPY --from=build /src/target/stage/lib /app/lib

ENV HOST=0.0.0.0 \
    PORT=8080 \
    BUILDER_DATA_DIR=/data \
    JAVA_TOOL_OPTIONS="-XX:MaxRAMPercentage=75"
USER app
VOLUME ["/data"]
EXPOSE 8080
HEALTHCHECK --interval=15s --timeout=3s --start-period=20s --retries=3 \
  CMD curl -fsS "http://127.0.0.1:${PORT}/health" >/dev/null || exit 1
ENTRYPOINT ["java", "-cp", "/app/lib/*", "builder.Main"]
