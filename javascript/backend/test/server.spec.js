import jsc from "jsverify";

describe("Addition", () => {
  it("should be commutative", () => {
    jsc.assert(
      jsc.forall("number", "number", (a, b) => {
        return a + b === b + a;
      }),
    );
  });

  it("should be associative", () => {
    jsc.assert(
      jsc.forall("number", "number", "number", (a, b, c) => {
        return a + (b + c) === a + b + c;
      }),
    );
  });
});
