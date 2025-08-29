const express = require("express");
const app = express();
const port = process.env.BACKEND_PORT;

app.get("/api/name", (req, res) => {
  res.send("JavaScript");
});

app.listen(port, () => {
  console.log(`Server listening on port ${port}`);
});
