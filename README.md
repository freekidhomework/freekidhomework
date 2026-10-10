# Math workbook site

A Quarto website that generates its own content: free 10-page math workbooks
(PDF) for Grades 1 to 4, in English and French, each with its own web page.

## Build it

You need R (any recent version, no packages) and Quarto 1.4 or newer
(included with current RStudio).

1. Open `mathsite.Rproj` in RStudio.
2. Build pane > **Render Website** (or run `quarto render` in the Terminal).
   Use `quarto preview` to see the site in a browser while you work.

Each render first runs `R/generate.R`, which writes the PDFs, preview images
and workbook pages into `en/` and `fr/`, then builds the site into `docs/`.
The first build takes a few minutes; later ones only rebuild what changed.

## Where things are

| File | What it controls |
|---|---|
| `_quarto.yml` | Site name (also printed on every PDF), domain, menu |
| `R/generate.R` | The list of workbooks (`skill(...)` entries), wording, languages |
| `typst/workbook.typ` | PDF design: fonts, colours, page layout |
| `index.qmd`, `fr/index.qmd` | Home pages with the searchable workbook grid |
| `styles.scss` | Website look |
| `_adsense.html`, `ads.txt` | Where the AdSense code goes once approved |

To add a workbook, copy one `skill(...)` block in `R/generate.R`, change its
id, titles and problem rule, and render again.

## Publish on GitHub Pages

1. Set `title` and `site-url` in `_quarto.yml`, then render.
2. Put your domain (one line, already set to `www.freekidhomework.com`) in a file named `CNAME` in this folder.
3. Push the whole folder to a GitHub repository.
4. On GitHub: Settings > Pages > Deploy from a branch > `main`, folder `/docs`.
5. At your domain registrar, point the domain to GitHub Pages as GitHub's instructions describe.
