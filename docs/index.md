# builder

Tracks renovation offers per project category (e.g. Gauben, Dämmung) and computes a running total.

## Running total rules

Every project has its own ordered list of categories; offers, favorites and work packages must
reference one of them. For every category:

- a **favorite** offer is counted;
- otherwise the **only** offer is counted;
- several offers without a favorite are reported as **needs decision** and nothing is counted;
- a category without offers is listed as **missing**.

All amounts must be in the project currency; `Project.from` rejects a mismatch (never converted)
and favorites that do not belong to their category, so computing totals cannot fail.

An offer without line items (Pauschalangebot) is accepted; otherwise its line items must sum to
net, and each line item's total must be `unitPrice × quantity` rounded HALF_UP to cents.

```scala mdoc
import builder.SampleData
import builder.totals.Totals

SampleData.project.map { p =>
  val t = Totals.compute(p)
  (t.net.show, t.gross.show, t.missingCategories.map(_.name.value), t.undecidedCategories.map(_.name.value))
}
```

## HTTP

- `GET /health`
- `GET /projects/{id}/totals` (JSON)
- `GET /` Projektübersicht (German HTML)
- `POST /ui/projects/{id}/categories/{categoryId}/favorite/{offerId}` (htmx; returns the re-rendered fragment)

Run with `sbt run` and try `curl localhost:8080/projects/demo/totals`.
