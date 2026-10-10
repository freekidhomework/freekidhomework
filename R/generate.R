# generate.R — builds every workbook PDF, its preview image and its landing page.
#
# Runs automatically before each `quarto render` (see _quarto.yml: pre-render),
# or by hand with source("R/generate.R") from the project folder.
# Base R only: no packages to install. PDFs are typeset with the Typst
# compiler that ships inside Quarto.
#
# To add a workbook: add one skill(...) entry in the SKILLS section.
# To add a language: add an entry to LANGS, STR, TOPICS and each skill's text.

root <- Sys.getenv("QUARTO_PROJECT_DIR", unset = getwd())
if (!isTRUE(l10n_info()[["UTF-8"]]))      # make sure accented text in this file is read as UTF-8
  for (loc in c("C.UTF-8", "en_US.UTF-8", "en_CA.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
setwd(root)

# ------------------------------------------------------------------ settings
LANGS  <- c("en", "fr")
PAGES  <- 8L                      # practice pages per workbook (+ cover + answers = 10)
PAPER  <- c(en = "us-letter", fr = "us-letter")   # use "a4" for European editions

site_title <- local({
  y <- readLines("_quarto.yml", warn = FALSE, encoding = "UTF-8")
  hit <- grep("^\\s+title:", y, value = TRUE)[1]
  trimws(gsub("^\\s+title:\\s*|\\s+#.*$|[\"']", "", hit))
})

STR <- list(
  en = list(grade = function(g) paste("Grade", g), dir = function(g) paste0("grade-", g),
            name = "Name", date = "Date", score = "Score", page = "Page",
            answers = "Answer key", tagline = "free to print and share",
            inside = c("8 practice pages", "%d problems in total", "Answer key on the last page"),
            pdf = "math workbook", free = "free printable PDF",
            download = "Download the PDF (10 pages)", other = "Aussi disponible en français",
            whats = "What's inside", equiv = "",
            body = "This free printable workbook has 8 practice pages with %d problems in total (%d per page) and a full answer key on the last page. No sign-up and no email needed: download the PDF and print it.",
            bullets = c("10 pages: cover, 8 practice pages and an answer key",
                        "%d problems per page, with room to write the answers",
                        "Every workbook contains different problems from page to page",
                        "US Letter size, prints cleanly in black and white"),
            more = "More %s workbooks", home = "/index.qmd"),
  fr = list(grade = function(g) paste0(g, if (g == 1) "re" else "e", " année"),
            dir = function(g) paste0("annee-", g),
            name = "Nom", date = "Date", score = "Résultat", page = "Page",
            answers = "Corrigé", tagline = "gratuit à imprimer et à partager",
            inside = c("8 pages d'exercices", "%d exercices au total", "Corrigé à la dernière page"),
            pdf = "cahier de maths", free = "PDF gratuit à imprimer",
            download = "Télécharger le PDF (10 pages)", other = "Also available in English",
            whats = "Contenu du cahier", equiv = c("CP", "CE1", "CE2", "CM1"),
            body = "Ce cahier gratuit à imprimer contient 8 pages d'exercices, soit %d exercices au total (%d par page), et un corrigé complet à la dernière page. Aucune inscription ni adresse courriel : téléchargez le PDF et imprimez-le.",
            bullets = c("10 pages : couverture, 8 pages d'exercices et un corrigé",
                        "%d exercices par page, avec de la place pour écrire les réponses",
                        "Les exercices changent d'une page à l'autre",
                        "Format Lettre, s'imprime bien en noir et blanc"),
            more = "Autres cahiers de %s", home = "/fr/index.qmd")
)

TOPICS <- list(
  add  = c(en = "Addition",       fr = "Addition"),
  sub  = c(en = "Subtraction",    fr = "Soustraction"),
  mul  = c(en = "Multiplication", fr = "Multiplication"),
  div  = c(en = "Division",       fr = "Division"),
  num  = c(en = "Number sense",   fr = "Sens du nombre"),
  frac = c(en = "Fractions",      fr = "Fractions"),
  dec  = c(en = "Decimals",       fr = "Nombres décimaux")
)

# ------------------------------------------------------------------- helpers
ri <- function(lo, hi) lo + floor(runif(1) * (hi - lo + 1))   # random integer, safe when lo == hi
pick <- function(x) x[ri(1, length(x))]
B <- "#blank()"
TIMES <- "×"; DIVIDE <- "÷"; MINUS <- "−"

# Locale-aware number formatting: 4,512 vs 4 512 and 3.5 vs 3,5
fmt <- function(n, lang) {
  s <- formatC(n, format = "d", big.mark = if (n >= 1000) "," else "")
  if (lang == "fr") s <- gsub(",", " ", s)
  s
}
dec <- function(tenths, lang) {
  s <- sprintf("%d.%d", tenths %/% 10, tenths %% 10)
  if (lang == "fr") sub(".", ",", s, fixed = TRUE) else s
}
digits <- function(n) rev(as.integer(strsplit(as.character(n), "")[[1]]))
carries <- function(a, b) {          # number of carries when adding a + b
  da <- digits(a); db <- digits(b); k <- max(length(da), length(db))
  da <- c(da, rep(0, k - length(da))); db <- c(db, rep(0, k - length(db)))
  carry <- 0; n <- 0
  for (i in seq_len(k)) { carry <- as.integer(da[i] + db[i] + carry >= 10); n <- n + carry }
  n
}
borrows <- function(a, b) {          # number of borrows when subtracting a - b
  da <- digits(a); db <- digits(b); db <- c(db, rep(0, length(da) - length(db)))
  borrow <- 0; n <- 0
  for (i in seq_along(da)) { borrow <- as.integer(da[i] - borrow < db[i]); n <- n + borrow }
  n
}
item <- function(q, a, key = q) list(q = q, a = as.character(a), key = key)
eq <- function(lhs, a) item(paste(lhs, "=", B), a, lhs)
vert <- function(a, b, op, ans, lang)
  item(sprintf("#vert([%s], [%s], [%s])", fmt(a, lang), fmt(b, lang), op), fmt(ans, lang), paste(a, op, b))
compare <- function(left, right, a, b)
  item(paste(left, "#h(0.3em) #circ #h(0.3em)", right), if (a < b) "<" else if (a > b) ">" else "=", paste(left, right))
seq_item <- function(start, step, lang, len = 6, nblank = 3) {
  v <- start + step * (0:(len - 1))
  hide <- sort(sample(3:len, nblank))
  shown <- vapply(v, fmt, "", lang = lang); shown[hide] <- "#blank(w: 2.6em)"
  item(paste(shown, collapse = ", #h(0.2em) "), paste(vapply(v[hide], fmt, "", lang = lang), collapse = ", "),
       paste(start, step))
}
until <- function(draw, ok) repeat { x <- draw(); if (ok(x)) return(x) }

# Draw n items for each page, avoiding repeats within the workbook where the
# skill has enough distinct problems, and within the page otherwise.
make_pages <- function(gen, n, lang) {
  seen_all <- character(); out <- vector("list", PAGES)
  for (p in seq_len(PAGES)) {
    items <- list(); seen_pg <- character(); tries <- 0
    while (length(items) < n) {
      it <- gen(p, lang); tries <- tries + 1
      if (it$key %in% seen_pg && tries < 300 * n) next
      if (it$key %in% seen_all && tries < 40 * n) next
      items[[length(items) + 1]] <- it
      seen_pg <- c(seen_pg, it$key); seen_all <- c(seen_all, it$key)
    }
    out[[p]] <- items
  }
  out
}

SKILLS <- list()
skill <- function(id, grade, topic, title, instr, desc, gen,
                  cols = 2, rows = 10, top = FALSE, anscols = 5) {
  if (!is.function(instr)) { txt <- instr; instr <- function(p, lang) txt[[lang]] }
  SKILLS[[id]] <<- list(id = id, grade = grade, topic = topic, title = title, instr = instr,
                        desc = desc, gen = gen, cols = cols, rows = rows, top = top, anscols = anscols)
}

# -------------------------------------------------------------------- SKILLS
# ---- Grade 1
skill("g1-add-10", 1, "add",
  c(en = "Addition within 10", fr = "Additions jusqu'à 10"),
  c(en = "Add. Write the answer on the line.", fr = "Additionne. Écris la réponse sur la ligne."),
  c(en = "Single-digit addition facts with sums up to 10, the first facts children learn by heart.",
    fr = "Les premières additions à mémoriser : des sommes de deux nombres dont le total ne dépasse pas 10."),
  function(p, lang) { a <- ri(0, 10); b <- ri(0, 10 - a); eq(paste(a, "+", b), a + b) })

skill("g1-sub-10", 1, "sub",
  c(en = "Subtraction within 10", fr = "Soustractions jusqu'à 10"),
  c(en = "Subtract. Write the answer on the line.", fr = "Soustrais. Écris la réponse sur la ligne."),
  c(en = "Subtraction facts starting from numbers up to 10, matching the addition facts learned in Grade 1.",
    fr = "Des soustractions à partir de nombres jusqu'à 10, en lien avec les additions apprises en début de primaire."),
  function(p, lang) { a <- ri(1, 10); b <- ri(0, a); eq(paste(a, MINUS, b), a - b) })

skill("g1-add-20", 1, "add",
  c(en = "Addition within 20", fr = "Additions jusqu'à 20"),
  c(en = "Add. Write the answer on the line.", fr = "Additionne. Écris la réponse sur la ligne."),
  c(en = "Addition facts with sums from 11 to 20, including crossing over ten.",
    fr = "Des additions dont la somme va de 11 à 20, avec passage de la dizaine."),
  function(p, lang) { a <- ri(2, 10); b <- ri(11 - a, min(10, 20 - a)); eq(paste(a, "+", b), a + b) })

skill("g1-sub-20", 1, "sub",
  c(en = "Subtraction within 20", fr = "Soustractions jusqu'à 20"),
  c(en = "Subtract. Write the answer on the line.", fr = "Soustrais. Écris la réponse sur la ligne."),
  c(en = "Subtraction from numbers 11 to 20, including crossing back over ten.",
    fr = "Des soustractions à partir de nombres de 11 à 20, avec passage de la dizaine."),
  function(p, lang) { a <- ri(11, 20); b <- ri(1, 10); eq(paste(a, MINUS, b), a - b) })

skill("g1-missing-10", 1, "add",
  c(en = "Missing addends within 10", fr = "Additions à trou jusqu'à 10"),
  c(en = "Write the missing number.", fr = "Écris le nombre manquant."),
  c(en = "Find the missing number in addition sentences up to 10, a first step toward subtraction and algebraic thinking.",
    fr = "Trouver le nombre manquant dans des additions jusqu'à 10 : un premier pas vers la soustraction."),
  function(p, lang) {
    s <- ri(2, 10); a <- ri(0, s)
    lhs <- if (runif(1) < 0.5) paste(a, "+", B) else paste(B, "+", a)
    item(paste(lhs, "=", s), s - a, paste(lhs, s))
  })

skill("g1-compare-100", 1, "num",
  c(en = "Comparing numbers to 100", fr = "Comparer les nombres jusqu'à 100"),
  c(en = "Write <, > or = in each circle.", fr = "Écris <, > ou = dans chaque cercle."),
  c(en = "Compare two numbers up to 100 using the symbols <, > and =.",
    fr = "Comparer deux nombres jusqu'à 100 avec les signes <, > et =."),
  function(p, lang) {
    a <- ri(1, 100); b <- if (runif(1) < 0.1) a else if (runif(1) < 0.4) max(1, min(100, a + pick(c(-10, -1, 1, 10)))) else ri(1, 100)
    compare(a, b, a, b)
  })

skill("g1-skip-count", 1, "num",
  c(en = "Skip counting by 2s, 5s and 10s", fr = "Compter par bonds de 2, 5 et 10"),
  c(en = "Find the pattern. Write the missing numbers.", fr = "Trouve la règle. Écris les nombres manquants."),
  c(en = "Number patterns counting forward by 2, 5 and 10 up to 100, which prepares children for multiplication.",
    fr = "Des suites de nombres par bonds de 2, 5 et 10 jusqu'à 100, pour préparer la multiplication."),
  function(p, lang) { s <- pick(c(2, 5, 10)); seq_item(s * ri(0, 100 / s - 5), s, lang) },
  cols = 1, anscols = 4)

skill("g1-doubles", 1, "add",
  c(en = "Doubles and near doubles", fr = "Doubles et presque-doubles"),
  c(en = "Add. Use the doubles you know.", fr = "Additionne. Sers-toi des doubles que tu connais."),
  c(en = "Doubles (6 + 6) and near doubles (6 + 7) up to 20, a mental-math strategy for fast addition.",
    fr = "Les doubles (6 + 6) et les presque-doubles (6 + 7) jusqu'à 20 : une stratégie de calcul mental."),
  function(p, lang) {
    a <- ri(1, 10); b <- if (a < 10) a + pick(c(0, 0, 1)) else a
    if (runif(1) < 0.5) { t <- a; a <- b; b <- t }
    eq(paste(a, "+", b), a + b)
  })

# ---- Grade 2
vadd <- function(lo, hi, max_sum, regroup) function(p, lang) {
  x <- until(function() c(ri(lo, hi), ri(lo, hi)),
             function(x) sum(x) <= max_sum && (carries(x[1], x[2]) > 0) == regroup)
  vert(x[1], x[2], "+", sum(x), lang)
}
vsub <- function(lo, hi, regroup) function(p, lang) {
  x <- until(function() sort(c(ri(lo, hi), ri(lo, hi)), decreasing = TRUE),
             function(x) x[1] > x[2] && (borrows(x[1], x[2]) > 0) == regroup)
  vert(x[1], x[2], MINUS, x[1] - x[2], lang)
}
V_ADD <- c(en = "Add. Line up the digits and start with the ones.", fr = "Additionne. Commence par les unités.")
V_SUB <- c(en = "Subtract. Start with the ones.", fr = "Soustrais. Commence par les unités.")

skill("g2-add-2d-plain", 2, "add",
  c(en = "2-digit addition without regrouping", fr = "Additions à 2 chiffres sans retenue"),
  V_ADD,
  c(en = "Column addition of two 2-digit numbers where no carrying is needed.",
    fr = "Des additions posées de deux nombres à 2 chiffres, sans retenue."),
  vadd(10, 89, 99, FALSE), cols = 4, rows = 5, top = TRUE)

skill("g2-add-2d-regroup", 2, "add",
  c(en = "2-digit addition with regrouping", fr = "Additions à 2 chiffres avec retenue"),
  V_ADD,
  c(en = "Column addition of two 2-digit numbers where the ones add up to 10 or more, so a ten must be carried.",
    fr = "Des additions posées de deux nombres à 2 chiffres où il faut reporter une dizaine (retenue)."),
  vadd(10, 89, 99, TRUE), cols = 4, rows = 5, top = TRUE)

skill("g2-sub-2d-plain", 2, "sub",
  c(en = "2-digit subtraction without regrouping", fr = "Soustractions à 2 chiffres sans retenue"),
  V_SUB,
  c(en = "Column subtraction of 2-digit numbers where no borrowing is needed.",
    fr = "Des soustractions posées de nombres à 2 chiffres, sans retenue (sans emprunt)."),
  vsub(10, 99, FALSE), cols = 4, rows = 5, top = TRUE)

skill("g2-sub-2d-regroup", 2, "sub",
  c(en = "2-digit subtraction with regrouping", fr = "Soustractions à 2 chiffres avec retenue"),
  V_SUB,
  c(en = "Column subtraction of 2-digit numbers where a ten must be borrowed (regrouped).",
    fr = "Des soustractions posées de nombres à 2 chiffres avec retenue (avec emprunt)."),
  vsub(10, 99, TRUE), cols = 4, rows = 5, top = TRUE)

skill("g2-missing-20", 2, "add",
  c(en = "Missing numbers within 20", fr = "Nombres manquants jusqu'à 20"),
  c(en = "Write the missing number.", fr = "Écris le nombre manquant."),
  c(en = "Addition and subtraction sentences up to 20 with one number missing.",
    fr = "Des additions et des soustractions à trou jusqu'à 20."),
  function(p, lang) {
    s <- ri(6, 20); a <- ri(1, s - 1)
    if (runif(1) < 0.5) item(paste(a, "+", B, "=", s), s - a, paste("a", a, s))
    else item(paste(s, MINUS, B, "=", a), s - a, paste("s", a, s))
  })

skill("g2-skip-count", 2, "num",
  c(en = "Skip counting to 1000", fr = "Compter par bonds jusqu'à 1000"),
  function(p, lang) if (p <= 4) c(en = "Count forward. Write the missing numbers.", fr = "Compte en avançant. Écris les nombres manquants.")[[lang]]
                    else c(en = "Count backward. Write the missing numbers.", fr = "Compte à rebours. Écris les nombres manquants.")[[lang]],
  c(en = "Number patterns by 2, 3, 5, 10 and 100 up to 1000, forward on the first four pages and backward on the last four.",
    fr = "Des suites de nombres par bonds de 2, 3, 5, 10 et 100 jusqu'à 1000, en avançant puis à rebours."),
  function(p, lang) {
    s <- pick(c(2, 3, 5, 10, 100)); start <- if (s == 100) ri(1, 400) else ri(1, 1000 - 5 * s - 1)
    if (p <= 4) seq_item(start, s, lang) else seq_item(start + 5 * s, -s, lang)
  },
  cols = 1, anscols = 4)

skill("g2-compare-1000", 2, "num",
  c(en = "Comparing numbers to 1000", fr = "Comparer les nombres jusqu'à 1000"),
  c(en = "Write <, > or = in each circle.", fr = "Écris <, > ou = dans chaque cercle."),
  c(en = "Compare 3-digit numbers using <, > and =, with many close pairs that differ by a single digit.",
    fr = "Comparer des nombres à 3 chiffres avec <, > et =, avec des nombres proches qui ne diffèrent que d'un chiffre."),
  function(p, lang) {
    a <- ri(100, 999); u <- runif(1)
    b <- if (u < 0.1) a else if (u < 0.6) max(100, min(999, a + pick(c(-100, -10, -1, 1, 10, 100)))) else ri(100, 999)
    compare(a, b, a, b)
  })

times <- function(tables, upto = 10) function(p, lang) {
  t <- pick(tables); k <- ri(1, upto)
  if (runif(1) < 0.5) eq(paste(t, TIMES, k), t * k) else eq(paste(k, TIMES, t), t * k)
}
MUL <- c(en = "Multiply. Write the answer on the line.", fr = "Multiplie. Écris la réponse sur la ligne.")

skill("g2-times-2-5-10", 2, "mul",
  c(en = "Multiplication tables: 2, 5 and 10", fr = "Tables de multiplication : 2, 5 et 10"),
  MUL,
  c(en = "The first times tables children learn: 2, 5 and 10, in mixed order.",
    fr = "Les premières tables de multiplication : 2, 5 et 10, dans le désordre."),
  times(c(2, 5, 10)))

# ---- Grade 3
skill("g3-times-2-5", 3, "mul",
  c(en = "Multiplication tables: 2 to 5", fr = "Tables de multiplication de 2 à 5"),
  MUL,
  c(en = "Mixed practice of the 2, 3, 4 and 5 times tables up to 10 times.",
    fr = "Un entraînement mélangé sur les tables de 2, 3, 4 et 5."),
  times(2:5))

skill("g3-times-6-9", 3, "mul",
  c(en = "Multiplication tables: 6 to 9", fr = "Tables de multiplication de 6 à 9"),
  MUL,
  c(en = "Mixed practice of the harder times tables: 6, 7, 8 and 9.",
    fr = "Un entraînement mélangé sur les tables les plus difficiles : 6, 7, 8 et 9."),
  times(6:9))

skill("g3-division-facts", 3, "div",
  c(en = "Division facts: dividing by 2 to 9", fr = "Divisions : tables de 2 à 9"),
  c(en = "Divide. Write the answer on the line.", fr = "Divise. Écris la réponse sur la ligne."),
  c(en = "Basic division facts with no remainders, the inverse of the times tables up to 9 times 10.",
    fr = "Des divisions sans reste, à partir des tables de multiplication de 2 à 9."),
  function(p, lang) { d <- ri(2, 9); q <- ri(1, 10); eq(paste(d * q, DIVIDE, d), q) })

skill("g3-add-3d", 3, "add",
  c(en = "3-digit addition with regrouping", fr = "Additions à 3 chiffres avec retenue"),
  V_ADD,
  c(en = "Column addition of 3-digit numbers with one or more carries, with sums up to 999.",
    fr = "Des additions posées de nombres à 3 chiffres avec une ou plusieurs retenues, jusqu'à 999."),
  vadd(100, 899, 999, TRUE), cols = 4, rows = 5, top = TRUE)

skill("g3-sub-3d", 3, "sub",
  c(en = "3-digit subtraction with regrouping", fr = "Soustractions à 3 chiffres avec retenue"),
  V_SUB,
  c(en = "Column subtraction of 3-digit numbers with one or more borrows.",
    fr = "Des soustractions posées de nombres à 3 chiffres avec une ou plusieurs retenues."),
  vsub(100, 999, TRUE), cols = 4, rows = 5, top = TRUE)

skill("g3-rounding", 3, "num",
  c(en = "Rounding to the nearest 10 and 100", fr = "Arrondir à la dizaine et à la centaine près"),
  function(p, lang) if (p <= 4) c(en = "Round each number to the nearest 10.", fr = "Arrondis chaque nombre à la dizaine près.")[[lang]]
                    else c(en = "Round each number to the nearest 100.", fr = "Arrondis chaque nombre à la centaine près.")[[lang]],
  c(en = "Round numbers up to 999 to the nearest ten (first four pages) and nearest hundred (last four pages).",
    fr = "Arrondir des nombres jusqu'à 999 à la dizaine près (quatre premières pages), puis à la centaine près."),
  function(p, lang) {
    m <- if (p <= 4) 10 else 100
    n <- until(function() ri(11, 989), function(n) n %% m != 0)
    item(paste(n, "#h(0.3em) #sym.arrow.r #h(0.3em) #blank(w: 3em)"), floor(n / m + 0.5) * m, paste(n, m))
  })

skill("g3-missing-factor", 3, "mul",
  c(en = "Missing factors", fr = "Multiplications à trou"),
  c(en = "Write the missing number.", fr = "Écris le nombre manquant."),
  c(en = "Find the missing factor in multiplication sentences from the 2 to 9 times tables, which links multiplication to division.",
    fr = "Trouver le facteur manquant dans des multiplications des tables de 2 à 9, pour faire le lien avec la division."),
  function(p, lang) {
    a <- ri(2, 9); b <- ri(1, 10)
    lhs <- if (runif(1) < 0.5) paste(a, TIMES, B) else paste(B, TIMES, a)
    item(paste(lhs, "=", a * b), b, paste(lhs, a * b))
  })

skill("g3-compare-fractions", 3, "frac",
  c(en = "Comparing fractions", fr = "Comparer des fractions"),
  c(en = "Write <, > or = in each circle.", fr = "Écris <, > ou = dans chaque cercle."),
  c(en = "Compare fractions with the same denominator (first four pages) and the same numerator (last four pages).",
    fr = "Comparer des fractions de même dénominateur (quatre premières pages), puis de même numérateur."),
  function(p, lang) {
    f <- function(n, d) sprintf("#frac(%d, %d)", n, d)
    if (p <= 4) { d <- pick(c(3, 4, 5, 6, 8, 10)); a <- ri(1, d - 1); b <- ri(1, d - 1)
      compare(f(a, d), f(b, d), a, b)
    } else { n <- ri(1, 3); a <- pick(c(4, 5, 6, 8, 10)); b <- pick(c(4, 5, 6, 8, 10))
      compare(f(n, a), f(n, b), b, a) }
  })

# ---- Grade 4
vmul <- function(lo, hi, lo2, hi2) function(p, lang) {
  a <- ri(lo, hi); b <- ri(lo2, hi2)
  vert(a, b, TIMES, a * b, lang)
}
V_MUL <- c(en = "Multiply. Show your work.", fr = "Multiplie. Montre ton travail.")

skill("g4-mult-2x1", 4, "mul",
  c(en = "Multiplying 2-digit by 1-digit numbers", fr = "Multiplications : 2 chiffres par 1 chiffre"),
  V_MUL,
  c(en = "Column multiplication of a 2-digit number by a 1-digit number.",
    fr = "Des multiplications posées d'un nombre à 2 chiffres par un nombre à 1 chiffre."),
  vmul(12, 99, 2, 9), cols = 4, rows = 4, top = TRUE)

skill("g4-mult-3x1", 4, "mul",
  c(en = "Multiplying 3-digit by 1-digit numbers", fr = "Multiplications : 3 chiffres par 1 chiffre"),
  V_MUL,
  c(en = "Column multiplication of a 3-digit number by a 1-digit number.",
    fr = "Des multiplications posées d'un nombre à 3 chiffres par un nombre à 1 chiffre."),
  vmul(101, 999, 2, 9), cols = 4, rows = 4, top = TRUE)

skill("g4-mult-2x2", 4, "mul",
  c(en = "Multiplying 2-digit by 2-digit numbers", fr = "Multiplications : 2 chiffres par 2 chiffres"),
  V_MUL,
  c(en = "Long multiplication of two 2-digit numbers, with plenty of space for the working.",
    fr = "Des multiplications posées de deux nombres à 2 chiffres, avec beaucoup de place pour les calculs."),
  vmul(12, 99, 12, 99), cols = 3, rows = 3, top = TRUE, anscols = 3)

skill("g4-division-remainders", 4, "div",
  c(en = "Division with remainders", fr = "Divisions avec reste"),
  c(en = "Divide. Write the quotient and the remainder (R).", fr = "Divise. Écris le quotient et le reste."),
  c(en = "Divide 2-digit numbers by 2 to 9 and write the quotient and the remainder.",
    fr = "Diviser des nombres à 2 chiffres par un nombre de 2 à 9 et écrire le quotient et le reste."),
  function(p, lang) {
    d <- ri(2, 9); n <- until(function() ri(11, 99), function(n) n %% d != 0)
    r <- c(en = "R", fr = "reste")[[lang]]
    item(paste(n, DIVIDE, d, "=", B, r, "#blank(w: 1.4em)"), paste(n %/% d, r, n %% d), paste(n, d))
  }, anscols = 5)

skill("g4-add-4d", 4, "add",
  c(en = "4-digit addition", fr = "Additions à 4 chiffres"),
  V_ADD,
  c(en = "Column addition of 4-digit numbers with regrouping, with sums up to 9,999.",
    fr = "Des additions posées de nombres à 4 chiffres avec retenues, jusqu'à 9 999."),
  vadd(1000, 8999, 9999, TRUE), cols = 4, rows = 4, top = TRUE, anscols = 4)

skill("g4-sub-4d", 4, "sub",
  c(en = "4-digit subtraction", fr = "Soustractions à 4 chiffres"),
  V_SUB,
  c(en = "Column subtraction of 4-digit numbers with regrouping across one or more places.",
    fr = "Des soustractions posées de nombres à 4 chiffres avec retenues."),
  vsub(1000, 9999, TRUE), cols = 4, rows = 4, top = TRUE, anscols = 4)

skill("g4-fractions-add-sub", 4, "frac",
  c(en = "Adding and subtracting fractions with like denominators",
    fr = "Additionner et soustraire des fractions de même dénominateur"),
  function(p, lang) if (p <= 4) c(en = "Add the fractions.", fr = "Additionne les fractions.")[[lang]]
                    else c(en = "Subtract the fractions.", fr = "Soustrais les fractions.")[[lang]],
  c(en = "Add (first four pages) and subtract (last four pages) fractions that share the same denominator. Answers are not simplified.",
    fr = "Additionner (quatre premières pages) puis soustraire des fractions de même dénominateur. Les réponses ne sont pas simplifiées."),
  function(p, lang) {
    d <- pick(c(5, 6, 7, 8, 9, 10, 12)); f <- function(n) sprintf("#frac(%d, %d)", n, d)
    if (p <= 4) { a <- ri(1, d - 2); b <- ri(1, d - 1 - a)
      item(paste(f(a), "+", f(b), "=", B), paste0(a + b, "/", d), paste(a, "+", b, d))
    } else { a <- ri(2, d - 1); b <- ri(1, a - 1)
      item(paste(f(a), MINUS, f(b), "=", B), paste0(a - b, "/", d), paste(a, "-", b, d)) }
  })

skill("g4-decimals-add-sub", 4, "dec",
  c(en = "Adding and subtracting decimals (tenths)", fr = "Additionner et soustraire des nombres décimaux (dixièmes)"),
  function(p, lang) if (p <= 4) c(en = "Add.", fr = "Additionne.")[[lang]] else c(en = "Subtract.", fr = "Soustrais.")[[lang]],
  c(en = "Add (first four pages) and subtract (last four pages) decimal numbers with one decimal place.",
    fr = "Additionner (quatre premières pages) puis soustraire des nombres décimaux à un chiffre après la virgule."),
  function(p, lang) {
    a <- ri(11, 99); b <- ri(11, 99)
    if (p <= 4) item(paste(dec(a, lang), "+", dec(b, lang), "=", B), dec(a + b, lang), paste(a, "+", b))
    else { x <- sort(c(a, b + 1), decreasing = TRUE); if (x[1] == x[2]) x[1] <- x[1] + 3
      item(paste(dec(x[1], lang), MINUS, dec(x[2], lang), "=", B), dec(x[1] - x[2], lang), paste(x[1], "-", x[2])) }
  })

# --------------------------------------------------------------- build steps
slugify <- function(x) {
  x <- chartr("àâäéèêëîïôöùûüçÀÂÉÈÊÎÔÙÛÇ", "aaaeeeeiioouuucAAEEEIOUUC", x)
  x <- gsub("[^a-z0-9]+", "-", tolower(x))
  gsub("^-|-$", "", x)
}
tstr <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')        # Typst string literal
ystr <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')        # YAML string literal
write_if_changed <- function(text, path) {
  text <- enc2utf8(paste(text, collapse = "\n"))
  old <- if (file.exists(path)) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else NA
  if (identical(old, text)) return(FALSE)
  con <- file(path, open = "wb"); on.exit(close(con))
  writeLines(text, con, useBytes = TRUE)
  TRUE
}
typst <- function(args) {
  q <- Sys.getenv("QUARTO_PATH", unset = Sys.which("quarto"))
  cmd <- if (nzchar(q)) list(q, c("typst", args)) else list(Sys.which("typst"), args)
  if (!nzchar(cmd[[1]])) stop("Could not find Quarto. Install it from https://quarto.org")
  out <- system2(cmd[[1]], cmd[[2]], stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(out, "status"))) stop(paste(out, collapse = "\n"))
}

paths <- function(s, lang) {
  dir <- file.path(lang, STR[[lang]]$dir(s$grade))
  slug <- slugify(s$title[[lang]])
  list(dir = dir, slug = slug, qmd = file.path(dir, paste0(slug, ".qmd")),
       pdf = paste0(slugify(STR[[lang]]$grade(s$grade)), "-", slug, ".pdf"), png = paste0(slug, ".png"),
       url = paste0("/", dir, "/", slug, ".qmd"))
}

build <- function(s, lang, order) {
  L <- STR[[lang]]; P <- paths(s, lang); n <- as.integer(s$cols * s$rows)
  dir.create(P$dir, recursive = TRUE, showWarnings = FALSE)
  dir.create("_build", showWarnings = FALSE)
  set.seed(sum(utf8ToInt(s$id)) * 7 + s$grade)        # same problems on every rebuild
  pages <- make_pages(s$gen, n, lang)
  grade <- L$grade(s$grade); title <- s$title[[lang]]

  typ <- c(
    '#import "/typst/workbook.typ": *',
    sprintf('#show: workbook.with(paper: %s, lang: %s, site: %s, tagline: %s, title: %s)',
            tstr(PAPER[[lang]]), tstr(lang), tstr(site_title), tstr(L$tagline), tstr(paste(title, "-", grade))),
    sprintf('#cover(grade: %s, title: %s, blurb: %s, name: %s, date: %s, inside: (%s,))',
            tstr(grade), tstr(title), tstr(s$desc[[lang]]), tstr(L$name), tstr(L$date),
            paste(tstr(sprintf(L$inside, n * PAGES)), collapse = ", ")),
    unlist(lapply(seq_len(PAGES), function(p) c(
      sprintf('#practice(title: %s, label: %s, instr: %s, score: %s, cols: %d, rows: %d, stacked: %s, (',
              tstr(title), tstr(sprintf("%s %d / %d", L$page, p, PAGES)), tstr(s$instr(p, lang)),
              tstr(L$score), s$cols, s$rows, if (s$top) "true" else "false"),
      paste0("  [", vapply(pages[[p]], `[[`, "", "q"), "],"), "))"))),
    sprintf('#answers(title: %s, label: %s, cols: %d, (', tstr(paste(L$answers, "-", title)), tstr(L$page), s$anscols),
    vapply(pages, function(pg) paste0("  (", paste(tstr(vapply(pg, `[[`, "", "a")), collapse = ", "), ",),"), ""),
    "))")
  typ_path <- file.path("_build", paste0(lang, "-", s$id, ".typ"))
  pdf <- file.path(P$dir, P$pdf); png <- file.path(P$dir, P$png)
  changed <- write_if_changed(typ, typ_path)
  if (changed || !file.exists(pdf) || !file.exists(png) ||
      file.mtime("typst/workbook.typ") > file.mtime(pdf)) {
    common <- c("--root", ".", "--font-path", "fonts")
    typst(c("compile", common, typ_path, pdf))
    typst(c("compile", common, "--pages", "2", "--ppi", "72", typ_path, png))
  }

  other <- setdiff(LANGS, lang)[1]
  topic <- TOPICS[[s$topic]][[lang]]
  equiv <- if (length(L$equiv) >= s$grade && nzchar(L$equiv[1])) sprintf(" (%s)", L$equiv[s$grade]) else ""
  qmd <- c("---",
    paste("title:", ystr(title)),
    paste("subtitle:", ystr(paste0(grade, equiv, " · ", L$pdf, " · ", L$free))),
    paste("description:", ystr(s$desc[[lang]])),
    paste("pagetitle:", ystr(paste0(title, " - ", grade, " ", L$pdf, " (", L$free, ")"))),
    sprintf("categories: [%s, %s]", ystr(grade), ystr(topic)),
    paste("image:", P$png),
    paste("order:", order),
    paste("lang:", lang),
    "---", "",
    "::: {.wb}",
    "::: {.wb-preview}",
    sprintf("[![%s](%s)](%s)", title, P$png, P$pdf),
    ":::",
    "::: {.wb-info}",
    sprintf("[%s](%s){.btn .btn-primary .btn-lg .wb-download}", L$download, P$pdf), "",
    paste(s$desc[[lang]], sprintf(L$body, n * PAGES, n)), "",
    paste("###", L$whats), "",
    paste("-", sprintf(L$bullets, n)), "",
    sprintf("[%s](%s){hreflang=%s} · [%s](%s)", L$other, paths(s, other)$url, other,
            sprintf(L$more, grade), paste0(L$home, "#category=", utils::URLencode(grade, reserved = TRUE))),
    ":::", ":::")
  write_if_changed(qmd, P$qmd)
  list(pdf = pdf, n = n, pages = pages)
}

order <- 0
for (s in SKILLS) {
  order <- order + 1
  for (lang in LANGS) build(s, lang, order)
}
message(sprintf("Built %d workbooks (%d skills x %d languages).", length(SKILLS) * length(LANGS), length(SKILLS), length(LANGS)))
