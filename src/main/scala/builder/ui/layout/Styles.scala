package builder.ui.layout

/** The page stylesheet. */
object Styles:
  private[ui] val styles =
    """body{font-family:system-ui,sans-serif;max-width:60rem;margin:2rem auto;padding:0 1rem;color:#222}
      |h1{margin-bottom:.25rem}section.kategorie{border:1px solid #ddd;border-radius:6px;padding:.5rem 1rem;margin:1rem 0}
      |table{border-collapse:collapse;width:100%}th,td{text-align:left;padding:.3rem .5rem;border-bottom:1px solid #eee}
      |td.betrag,th.betrag{text-align:right;font-variant-numeric:tabular-nums}
      |.status{display:inline-block;padding:.1rem .5rem;border-radius:4px;font-size:.85rem;margin-left:.5rem}
      |.gezaehlt{background:#d9f2d9}.offen{background:#ffe9b3}.fehlt{background:#f5d0d0}
      |.favorit{font-weight:bold}.summe{background:#f4f6fa;padding:1rem;border-radius:6px}
      |.hinweis{color:#666;font-size:.9rem}.fehlermeldung{background:#f5d0d0;padding:.5rem 1rem;border-radius:6px}
      |.upload{border-top:1px dashed #ccc;margin-top:.75rem;padding-top:.5rem}.upload h3{font-size:1rem;margin:.25rem 0}
      |.unsicher{background:#fff3cd}.flag{font-size:.8rem;padding:0 .3rem;border-radius:3px;white-space:nowrap}
      |.flag.niedrig{background:#f0ad4e;color:#000}.flag.mittel{background:#eee}.seite{color:#666;font-size:.8rem;white-space:nowrap}
      |.warnung{background:#fff3cd;padding:.5rem 1rem;border-radius:6px}.abweichung{background:#f5d0d0;padding:.5rem 1rem;border-radius:6px}
      |table.felder th{width:10rem}form.hochladen{margin:.5rem 0}.htmx-indicator{display:none}.htmx-request .htmx-indicator,.htmx-request.htmx-indicator{display:inline}
      |table.matrix td,table.matrix th{vertical-align:top;border:1px solid #e3e3e3}table.matrix th.leistung{min-width:9rem}
      |.st-enthalten{background:#e3f4e3}.st-nicht{background:#f3f3f3}.st-ausgeschlossen{background:#f8dede}.st-unklar{background:#fff3cd}
      |td.pruefen,div.pruefen{outline:2px solid #f0ad4e;outline-offset:-2px}form.aendern{display:inline-block;margin:.15rem .25rem .15rem 0}
      |table.matrix select{font-size:.8rem}details summary{cursor:pointer}section.leistungen{margin-top:1rem}
      |nav.tabs ul{list-style:none;display:flex;flex-wrap:wrap;gap:.25rem;padding:0;margin:1rem 0 0;border-bottom:2px solid #ccc}
      |nav.tabs a{display:block;padding:.4rem .9rem;border:1px solid #ccc;border-bottom:none;border-radius:6px 6px 0 0;text-decoration:none;color:#222;background:#f4f6fa}
      |nav.tabs a.aktiv{background:#fff;font-weight:bold;border-color:#888;position:relative;top:2px}
      |nav.tabs a.gezaehlt{border-top:3px solid #5cb85c}nav.tabs a.offen{border-top:3px solid #f0ad4e}nav.tabs a.fehlt{border-top:3px solid #d9534f}
      |table.rangliste td{vertical-align:top}td.gesamt{font-size:1.05rem}section.gewichtung{margin:1rem 0}section.gewichtung input[type=number]{width:4rem}""".stripMargin
