import { greet } from "./greet.mjs";

window.addEventListener("load", () => {
  fetch("/api/name")
    .then((response) => {
      if (!response.ok) {
        throw new Error(`Response status: ${response.status}`);
      }
      return response.text();
    })
    .then((name) => {
      console.log(greet(name));
    })
    .catch((error) => {
      console.error(error);
    });
});
