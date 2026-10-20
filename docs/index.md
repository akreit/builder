# builder

Tracks renovation offers per trade and computes a running total.

## Running total rules

For every category in scope (it has a work package, an offer or a favorite):

- a **favorite** offer is counted;
- otherwise the **only** offer is counted;
- several offers without a favorite are reported as **needs decision** and nothing is counted;
- a category without offers is listed as **missing**.

All amounts must be in the project currency; a mismatch is an error, never converted.

```scala mdoc
import builder.SampleData
import builder.totals.Totals

SampleData.project.map(p =>
  Totals.compute(p).map(t => (t.net.show, t.gross.show, t.missingCategories, t.undecidedCategories))
)
```

## HTTP

- `GET /health`
- `GET /projects/{id}/totals`

Run with `sbt run` and try `curl localhost:8080/projects/demo/totals`.
