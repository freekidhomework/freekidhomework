// Shared layout for every workbook. Edit colours, fonts and spacing here;
// R/generate.R only supplies the content.

#let ink = rgb("#1b1f24")
#let accent = rgb("#2563eb")
#let soft = rgb("#e8f0fe")
#let muted = rgb("#6b7280")

// Answer blank, comparison circle, stacked fraction
#let blank(w: 2.2em) = box(width: w, height: 1em, baseline: 0.2em, stroke: (bottom: 0.9pt + ink))
#let circ = box(baseline: 0.3em, circle(radius: 0.62em, stroke: 0.9pt + ink))
#let frac(a, b) = box(baseline: 0.85em, inset: (x: 0.1em), align(center, stack(
  spacing: 0.22em, [#a], line(length: 1.1em, stroke: 0.9pt + ink), [#b],
)))

// Column (stacked) arithmetic: a over b with an operator and a rule underneath
#let vert(a, b, op) = context {
  let big(x) = text(size: 21pt, tracking: 1.5pt, x)
  let first = box(big(a))
  let second = box(big[#op#h(0.7em)#b])
  let w = calc.max(measure(first).width, measure(second).width) + 6pt
  box(width: w, align(right, stack(spacing: 9pt, first, second, line(length: 100%, stroke: 1pt + ink))))
}

#let workbook(paper: "us-letter", lang: "en", site: "", tagline: "", title: "", body) = {
  set document(title: title, author: site)
  set page(
    paper: paper,
    margin: (x: 2cm, top: 1.7cm, bottom: 2cm),
    footer: context {
      set text(size: 9pt, fill: muted)
      grid(columns: (1fr, auto), [#site #sym.dot.c #tagline], counter(page).display())
    },
  )
  set text(font: "Andika", size: 17pt, fill: ink, lang: lang)
  body
}

#let cover(grade: "", title: "", blurb: "", name: "", date: "", inside: ()) = {
  v(1.6cm)
  box(fill: accent, inset: (x: 14pt, y: 9pt), radius: 6pt, text(fill: white, weight: "bold", size: 16pt, grade))
  v(0.5cm)
  par(leading: 0.45em, text(size: 38pt, weight: "bold", title))
  v(0.2cm)
  text(size: 14pt, fill: muted, blurb)
  v(1fr)
  block(width: 100%, fill: soft, radius: 14pt, inset: 1.1cm, align(center, text(
    size: 64pt, weight: "bold", fill: accent, tracking: 18pt, "+ − × ÷",
  )))
  v(1fr)
  set text(size: 14pt)
  grid(
    columns: (auto, 1fr, auto, 5cm), column-gutter: 8pt, align: bottom,
    name, line(length: 100%, stroke: 0.8pt + ink), [#h(10pt)#date], line(length: 100%, stroke: 0.8pt + ink),
  )
  v(0.8cm)
  set text(size: 11.5pt, fill: muted)
  for it in inside [#sym.bullet #h(4pt) #it \ ]
  pagebreak()
}

#let page-head(title, label) = {
  grid(
    columns: (1fr, auto), align: horizon,
    text(weight: "bold", size: 15pt, title),
    if label != "" { box(fill: soft, inset: (x: 10pt, y: 5pt), radius: 5pt, text(size: 11pt, fill: accent, weight: "bold", label)) },
  )
  v(-6pt)
  line(length: 100%, stroke: 1.2pt + accent)
}

#let practice(title: "", label: "", instr: "", score: "", cols: 2, rows: 10, stacked: false, items) = {
  page-head(title, label)
  text(size: 13pt, instr)
  v(4pt)
  block(height: 1fr, width: 100%, grid(
    columns: (1fr,) * cols, rows: (1fr,) * rows,
    align: if stacked { left + top } else { left + horizon },
    ..items.enumerate().map(((i, it)) => grid(
      columns: (1.9em, auto), align: (left + top, left),
      text(size: 9pt, fill: muted)[#(i + 1)],
      if stacked { pad(top: 10pt, it) } else { it },
    )),
  ))
  align(right, text(size: 12pt)[#score #h(4pt) #blank(w: 2.4em) / #items.len()])
  pagebreak()
}

#let answers(title: "", label: "", cols: 5, data) = {
  page-head(title, "")
  v(4pt)
  for (p, ans) in data.enumerate() {
    block(breakable: false, below: 13pt, width: 100%)[
      #text(weight: "bold", size: 10.5pt, fill: accent)[#label #(p + 1)]
      #v(-5pt)
      #set text(size: 9.5pt)
      #grid(
        columns: (1fr,) * cols, row-gutter: 5.5pt,
        ..ans.enumerate().map(((i, a)) => [#text(fill: muted)[#(i + 1).] #a]),
      )
    ]
  }
}
