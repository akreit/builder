# syntax=docker/dockerfile:1

# ---- Build stage: compile with sbt 2 and stage all runtime jars ----
FROM eclipse-temurin:21-jdk-noble AS build
ARG SBT_VERSION=2.0.11
RUN curl -fsSL "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" \
      | tar -xz -C /opt
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
FROM eclipse-temurin:21-jre-noble
RUN groupadd --system app && useradd --system --gid app --home-dir /app app \
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
