import { expect } from "chai";
import { greet } from "../src/greet.mjs";

describe("Greeting", () => {
  const name = "JavaScript";

  it("should say hello", () => {
    expect(greet(name).toLowerCase()).to.contain("hello");
  });

  it("should contain the name", () => {
    expect(greet(name)).to.contain(name);
  });
});
