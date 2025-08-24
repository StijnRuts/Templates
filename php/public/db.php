<?php

$dbconn =
  pg_connect(
    "host=".getenv("DB_HOST").
    " dbname=".getenv("DB_NAME").
    " user=".getenv("DB_USER").
    " password=".getenv("DB_PASSWORD")
  ) or die('Could not connect: ' . pg_last_error());

$query = 'SELECT * FROM Authors';
$result = pg_query($dbconn, $query) or die('Query failed: ' . pg_last_error());

echo "<table>\n";
while ($line = pg_fetch_array($result, null, PGSQL_ASSOC)) {
  echo "\t<tr>\n";
  foreach ($line as $col_value) {
    echo "\t\t<td>$col_value</td>\n";
  }
  echo "\t</tr>\n";
}
echo "</table>\n";

pg_free_result($result);
pg_close($dbconn);
