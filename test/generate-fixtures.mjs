/**
 * Compiles the Elm fixture generator and runs it to produce fixtures.json.
 * This bridges Elm output → JS cross-reference tests for true end-to-end testing.
 */
import { execSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import vm from "node:vm";

const __dirname = dirname(fileURLToPath(import.meta.url));

// Step 1: Compile the Elm fixture generator
console.log("Compiling Elm fixture generator...");
execSync("npx elm make src/GenerateFixtures.elm --output=generate-fixtures.js", {
  cwd: __dirname,
  stdio: "inherit",
});

// Step 2: Load the compiled Elm app using Node's vm module
const elmCode = readFileSync(join(__dirname, "generate-fixtures.js"), "utf-8");
const context = vm.createContext({ setTimeout, clearTimeout, setInterval });
vm.runInContext(elmCode, context);
const Elm = context.Elm;

// Step 3: Initialize the Elm app and capture port output
const fixtures = await new Promise((resolve, reject) => {
  const timeout = setTimeout(() => reject(new Error("Elm app timed out")), 5000);
  const app = Elm.GenerateFixtures.init();
  app.ports.fixtures.subscribe((data) => {
    clearTimeout(timeout);
    resolve(data);
  });
});

// Step 4: Write fixtures to JSON
const outputPath = join(__dirname, "fixtures.json");
writeFileSync(outputPath, JSON.stringify(fixtures, null, 2));
console.log(`Wrote ${fixtures.length} fixtures to ${outputPath}`);
