# builder

Tracks renovation offers per project category (Dach, Fenster, Fassadendämmung, Elektrik, Heizung, Innenausbau), one tab per category, and computes a running total.

## Running total rules

Every project has its own ordered list of categories; offers, favorites and work packages must
reference one of them. For every category:

- a **favorite** offer is counted;
- otherwise the **only** offer is counted;
- several offers without a favorite are reported as **needs decision** and nothing is counted;
- a category without offers is listed as **missing**.

All amounts must be in the project currency; `Project.from` rejects a mismatch (never converted)
and favorites that do not belong to their category, so computing totals cannot fail.

An offer without line items (Pauschalangebot) is accepted. Line items may lack quantity or price
(`QuantityInfo.Missing` / `PriceInfo.Missing`): the offer is still accepted, counted in totals /
favorites / ranking via its extracted net and gross, and the missing fields are shown empty and
flagged ("Menge fehlt" / "Preis fehlt") in the offer detail view. Positions without a price are
excluded from per-Leistung sums in the comparison matrix. Each matrix cell that maps line
items shows those positions in place as a native `<details>` toggle (collapsed by default),
reusing the same markup as the offer detail view; missing Menge/Preis only show the per-position
flags ("Menge fehlt" / "Preis fehlt"), never a consistency warning. The header then only confirms
net/VAT/gross (`lineSumCheck` is empty — never "Positionen … stimmen überein"). When every line
item has a known total, those totals must sum to net (`LineSumCheck.Consistent`); when quantity,
unit price and total are all known, the total must be `unitPrice × quantity` rounded HALF_UP to
cents.

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
- `GET /` redirects (303) to the first category tab
- `GET /projekte/{projectId}/kategorien/{categoryId}` page of one category (German HTML; unknown project or category: 404)
- `POST /ui/projects/{id}/categories/{categoryId}/favorite/{offerId}` (htmx; returns the re-rendered fragment)

Run with `sbt run` and try `curl localhost:8080/projects/demo/totals`.
