#source("/home/nathan/Dropbox/901_functions/Custom header.R")
source("C:\\Users\\natha\\Dropbox\\901_functions\\Custom header.R")


library(glue)

# Constants for dynamic values
league_type <- "standard"
number_of_players <- 500
table_title <- paste("PFF ", league_type, " Cheat Sheet", sep = "")
csv_filename <- gsub("\\s+", "_", tolower(table_title)) # e.g., "cheat_sheet"

url <-
  "https://docs.google.com/spreadsheets/d/1KbwN3IDUxRzDx9vCMz2Xe4o3eQj7574wFCbW8VrdeFw/edit#gid=80961760"
players <- read_sheet(url, sheet = "players")
teams <- read_sheet(url, sheet = "teams")

players <- left_join(
  x = players,
  y = teams,
  by = c("current_team" = "team_last_name...2")
)

players <- players %>%
  mutate(
    rank = case_when(
      league_type == "PPR" ~ players$ppr_rank,
      league_type == "half" ~ players$half_rank,
      league_type == "standard" ~ players$standard_rank,
      league_type == "superflex" ~ players$superflex_rank,
      league_type == "dynasty" ~ players$dynasty_rank,
      league_type == "dynasty superflex" ~ players$dynasty_superflex_rank,
      league_type == "dynasty rookie" ~ players$dynasty_superflex_rank,
      league_type == "dynasty rookie superflex" ~
        players$dynasty_superflex_rookie_rank,
      TRUE ~ players$ppr_rank
    )
  )

players$player_profile_url <- ifelse(
  is.na(
    players$player_profile_url
  ),
  players$pff_player_url,
  players$player_profile_url
)

players <- select(
  players,
  rank,
  position,
  name,
  current_team,
  player_profile_url,
  url
)
players <- filter(players, players$rank <= number_of_players) %>%
  arrange(rank)

# Build <tbody> rows from dataframe
rows <- apply(players, 1, function(r) {
  glue(
    '<tr>\n',
    '  <td>{r["rank"]}</td>\n',
    '  <td>{r["position"]}</td>\n',
    '  <td><a href="{r["player_profile_url"]}">{r["name"]}</a></td>\n',
    '  <td><a href="{r["url"]}">{r["current_team"]}</a></td>\n',
    '</tr>'
  )
})

tbody_html <- paste(rows, collapse = "\n")

html_lines <- c(
  '<!DOCTYPE html>',
  '<html lang="en">',
  '<head>',
  '  <meta charset="UTF-8">',
  '  <meta name="viewport" content="width=device-width, initial-scale=1">',
  sprintf('  <title>%s</title>', table_title),
  '  <link href="https://fonts.googleapis.com/css2?family=Archivo&display=swap" rel="stylesheet">',
  '  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">',
  '  <link rel="stylesheet" href="https://cdn.datatables.net/1.13.6/css/dataTables.bootstrap5.min.css">',
  '  <link rel="stylesheet" href="https://cdn.datatables.net/buttons/2.4.1/css/buttons.bootstrap5.min.css">',
  '  <style>',
  "    body, table { font-family: 'Archivo', sans-serif; }",
  '    #example th, #example td { text-align: center; }',
  '    #example a { color: #0d6efd; text-decoration: none; }',
  '    #example a:hover { text-decoration: underline; }',
  '    #example thead input, #example thead select { width:100%; box-sizing:border-box; }',
  '    .dataTables_filter { display: none; }',
  '    @media (max-width: 768px) {',
  '      #example th, #example td { font-size: 12px; padding: 4px; }',
  '      #example thead input, #example thead select { font-size: 12px; padding: 2px; }',
  '    }',
  '  </style>',
  '</head>',
  '<body class="p-4">',
  '  <div class="table-responsive">',
  '    <table id="example" class="table table-striped table-hover">',
  '      <thead class="table-light">',
  '        <tr><th>Rank</th><th>Position</th><th>Name</th><th>Team</th></tr>',
  '        <tr class="filters">',
  '          <th><input type="text" class="form-control form-control-sm" placeholder="Filter Rank"></th>',
  '          <th></th>',
  '          <th><input type="text" class="form-control form-control-sm" placeholder="Filter Name"></th>',
  '          <th></th>',
  '        </tr>',
  '      </thead>',
  '      <tbody>',
  tbody_html,
  '      </tbody>',
  '    </table>',
  '  </div>',
  '  <script src="https://code.jquery.com/jquery-3.7.0.min.js"></script>',
  '  <script src="https://cdn.datatables.net/1.13.6/js/jquery.dataTables.min.js"></script>',
  '  <script src="https://cdn.datatables.net/1.13.6/js/dataTables.bootstrap5.min.js"></script>',
  '  <script src="https://cdn.datatables.net/buttons/2.4.1/js/dataTables.buttons.min.js"></script>',
  '  <script src="https://cdn.datatables.net/buttons/2.4.1/js/buttons.bootstrap5.min.js"></script>',
  '  <script src="https://cdnjs.cloudflare.com/ajax/libs/jszip/3.10.1/jszip.min.js"></script>',
  '  <script src="https://cdn.datatables.net/buttons/2.4.1/js/buttons.html5.min.js"></script>',
  '  <script>',
  '    $(document).ready(function() {',
  '      var table = $("#example").DataTable({ dom: "B t",',
  sprintf(
    '        buttons: [{ extend: "csvHtml5", text: "Export to CSV", filename: "%s" }],',
    csv_filename
  ),
  '        paging: false, ordering: true, info: false, orderCellsTop: true',
  '      });',
  '      // text filters for columns 0 & 2',
  '      $("#example thead tr.filters th").each(function(i) {',
  '        var $inp = $("input", this); if ($inp.length) { $inp.on("keyup change", function() { table.column(i).search(this.value).draw(); }); }',
  '      });',
  '      // column dropdowns for Position & Team',
  '      function addDropdown(colIndex, placeholder) {',
  '        var col = table.column(colIndex);',
  '        var vals = col.data().map(function(d) { return $("<div>").html(d).text(); }).toArray();',
  '        vals = Array.from(new Set(vals)).sort();',
  '        var sel = $("<select class=\\"form-select form-select-sm\\"></select>");',
  '        sel.append($("<option>").val("").text(placeholder));',
  '        $.each(vals, function(_, v) { sel.append($("<option>").val(v).text(v)); });',
  '        sel.appendTo($("#example thead tr.filters th").eq(colIndex));',
  '        sel.on("change", function() { col.search(this.value ? "^" + this.value + "$" : "", true, false).draw(); });',
  '      }',
  '      addDropdown(1, "Filter Position");',
  '      addDropdown(3, "Filter Team");',
  '    });',
  '  </script>',
  '</body>',
  '</html>'
)

file_name <- paste(
#  "/home/nathan/Dropbox/403_article_creation/html/cheat_sheet",
  "C:\\Users\\natha\\Dropbox\\403_article_creation\\html\\cheat_sheet",
  league_type,
  "body",
  ".html",
  sep = "_"
)

# Write HTML and CSV to disk
writeLines(html_lines, file_name)
