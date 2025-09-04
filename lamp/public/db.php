<?php

$dbconn = new mysqli(
  getenv("DB_HOST"),
  getenv("DB_USER"),
  getenv("DB_PASSWORD"),
  getenv("DB_NAME")
);

if ($dbconn->connect_error) {
  die('Could not connect: ' . $dbconn->connect_error);
}

$query = 'SELECT * FROM Authors';
$result = $dbconn->query($query);

if (!$result) {
  die('Query failed: ' . $dbconn->error);
}

echo "<table>\n";
while ($line = $result->fetch_assoc()) {
  echo "\t<tr>\n";
  foreach ($line as $col_value) {
    echo "\t\t<td>$col_value</td>\n";
  }
  echo "\t</tr>\n";
}
echo "</table>\n";

$result->free();
$dbconn->close();
