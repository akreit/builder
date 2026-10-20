# Deployment research: Vercel fit for the builder backend

_Researched 2026-10-20 from current Vercel docs. Nothing has been deployed or provisioned yet._

App profile: Scala 3 on the JVM (JDK 21), http4s Ember always-on server, Tapir, sbt 2. It needs Postgres and
object storage for uploaded plan/offer PDFs and images, plus logs.

## TL;DR

- **Vercel has no native Java/Scala runtime.** The native runtimes are Node.js (default), Bun and Python, plus Edge, which is deprecated.
- **A JVM can still run there through a container.** The Container Runtime is in Beta on all plans. You add a `Dockerfile.vercel`
  file and Vercel runs the OCI image as a Function. That Function autoscales, must be **stateless**, and **scales to zero after 5 minutes idle**
  in production (30 s in preview). Every cold start then means booting a JVM plus the Ember/cats-effect stack, which takes seconds without
  extra tuning. This is **not an always-on server**. Each request also has the normal Function limits: max duration 300 s on Hobby and
  800 s on Pro, and 4.5 MB request and response bodies, which matters for PDF uploads. Static IPs and Secure Compute are not supported.
- **Daemons do not fit either.** Daemons (Public Beta) are always-on containers, but they **do not receive inbound HTTP/TCP**. They are built for queue workers.
- **Services (Beta)** only bundles several frontends and backends into one project with routing. They still run as Functions or containers, with the same scaling rules.
- **Verdict:** Vercel is a poor fit for an always-on JVM Ember backend. The realistic pattern is to run the **backend container
  on a container host that keeps instances running**, such as Fly.io, Render, Google Cloud Run with min-instances >= 1, or Railway.
  Use **Vercel only for a future web frontend**, rewriting `/api/*` to the backend. You could run on Vercel's Container Runtime if cold
  starts are acceptable or the JVM is tuned (for example CDS/AppCDS or a GraalVM native-image build). Treat that as an experiment, not the default.

## 1. Compute on Vercel

| Option | Inbound HTTP | Always on | Fit for Ember JVM |
|---|---|---|---|
| Native Functions (Node/Bun/Python) | yes | no | no (no JVM runtime) |
| Container Runtime (`Dockerfile.vercel`, Beta) | yes, on `$PORT` (default 80) | no, scales to zero after 5 min idle | works but is request-scoped; cold-start penalty |
| Services (Beta) | yes, via rewrites | no (same as above) | only groups the pieces |
| Daemons (Public Beta) | **no** | yes, fixed instances per region | only for background workers |

Container notes: images are pushed to Vercel Container Registry (VCR), and the server gets SIGTERM with a 30 s grace period.
Container stdout and stderr appear in Runtime Logs.

Sources:
- https://vercel.com/docs/functions/runtimes
- https://vercel.com/docs/functions/container-images
- https://vercel.com/kb/guide/vercel-functions-containers-daemons (it lists "a Java API" as a container use case)
- https://vercel.com/docs/daemons , https://vercel.com/docs/daemons/limits
- https://vercel.com/docs/services
- https://vercel.com/docs/functions/limitations

## 2. Storage and logs

- **Postgres:** Vercel Postgres has been retired (sunset). Postgres now comes through Marketplace integrations, with Neon as the default.
  Vercel bills these through unified billing and injects `DATABASE_URL` and related variables. Any Postgres works from an external backend,
  for example Neon directly, Fly Postgres, or Render Postgres.
  Neon Free tier: 100 CU-hours per project, 1 GB storage per project (20 GB per account), scale-to-zero after 5 min that you cannot turn off, and 5 GB egress.
  Neon Launch: $0.106 per CU-hour, $0.35 per GB-month, no minimum. https://neon.com/pricing , https://vercel.com/docs/storage
- **Vercel Blob (object storage):** offers public or private stores, files up to 5 TB, and multipart uploads. Clients upload directly, which avoids the 4.5 MB Function body limit.
  Hobby allowance: 1 GB storage, 10K simple ops, 2K advanced ops and 10 GB transfer per month. Past that, access is **blocked for 30 days**.
  Pro: $0.023 per GB-month, $0.40 per 1M simple ops, $5 per 1M advanced ops, and transfer from about $0.05/GB (iad1).
  The backend can use the Blob REST API, since there is no JVM SDK. An S3-compatible store such as R2, S3 or Tigris may be simpler from Scala.
  https://vercel.com/docs/vercel-blob/usage-and-pricing
- **Logs:** Runtime Logs are kept for 1 hour on Hobby, 1 day on Pro, and 30 days with Observability Plus ($1.20 per 1M events).
  Limits are 256 lines per request and 1 MB per request. **Log Drains are Pro only, at $0.50/GB.** For a backend hosted elsewhere, use that host's logs
  or a drain to a log service. https://vercel.com/docs/logs/runtime , https://vercel.com/pricing

## 3. Manual steps for Andreas (not done by any agent)

1. Create a Vercel account or team and pick a plan. **Hobby is for non-commercial use only.** A business product needs Pro: $20/month per developer seat, with a $20 usage credit included.
2. Install the Vercel GitHub App on `akreit/builder`, or on a frontend repo, and create and import the project.
3. Install Marketplace integrations (Neon, and Blob, which is first-party) and connect them to the project. This accepts the provider's terms and billing.
4. Add environment variables and secrets (DB URL, Blob token, app secrets) under Project Settings → Environment Variables, using the Secret/sensitive type.
   https://vercel.com/docs/environment-variables
5. Set up billing: a payment method, the Spend Management budget (default on-demand budget is $200), and optionally a hard pause.
6. If you follow the recommendation: create an account on the backend host (Fly.io, Render or Cloud Run), add billing, set secrets there, and point DNS or the Vercel rewrite at it.

## 4. Cost items (USD, excl. tax). Source: https://vercel.com/pricing unless noted

- Plans: Hobby $0 (non-commercial, hard caps, no overage purchase). Pro $20/month per developer seat with a $20 usage credit, then pay as you go.
- Functions (also used by containers): Hobby includes 4 h Active CPU, 360 GB-h memory and 1M invocations. Pro: $0.128 per CPU-hour, $0.0106 per GB-hour, $0.60 per 1M invocations.
- Service Requests (Beta): 1M included, then $0.50 per 1M (Pro).
- Daemons: Hobby includes 5 h CPU and 420 GB-h. Pro: $0.128 per CPU-hour, $0.0212 per GB-hour.
- Container Registry storage: 10 GB included on Hobby, $0.10 per GB-month on Pro.
- CDN requests: 1M (Hobby) or 10M (Pro) included, then from $2 per 1M. Fast Data Transfer: 100 GB or 1 TB included, then from $0.15/GB. Fast Origin Transfer: 10 GB included, then from $0.06/GB.
- Build minutes: Basic is included on Hobby. Pro from $0.007/min. A JVM/sbt container build will use these.
- Blob: see section 2. Neon: see section 2. It is billed through Vercel when installed from the Marketplace.
- Observability Plus: $1.20 per 1M events. Log Drains: $0.50/GB. Static IPs: $100 per project per month (not usable with containers).
- Optional extras: Password Protection $20 per project per month, SAML SSO $300/month, HIPAA BAA $350/month.
- External backend host if used. Check current pricing before committing: https://fly.io/docs/about/pricing/ ,
  https://render.com/pricing , https://cloud.google.com/run/pricing (min-instances >= 1 bills for idle time).
