const API_BASE = "scattered-lebbie-steven-illg-it-f8b8abdf.koyeb.app/";

const $in = document.getElementById("in");
const $out = document.getElementById("out");
const $inHL = document.getElementById("inHL");
const $outHL = document.getElementById("outHL");

const $inGutter = document.getElementById("inGutter");
const $outGutter = document.getElementById("outGutter");

const $go = document.getElementById("translate");
const $copy = document.getElementById("copy");
const $download = document.getElementById("download");
const $status = document.getElementById("status");
const $led = document.getElementById("led");
const $autoBadge = document.getElementById("autoBadge");

const $examples = document.getElementById("examples");
const $loadExample = document.getElementById("loadExample");
const $reset = document.getElementById("reset");

const EXAMPLES = {
  snippet_print: `System.out.println(name);`,
  snippet_if: `int y = x + 1;
if (y > 10) {
  System.out.println(y);
} else {
  System.out.println(0);
}`,
  class_simple: `public class Demo {
  int plusOne(int x) { 
  return x + 1; 
  }
}`,
  class_hello: `/** HelloWorldApp prints "Hello World!" */
public class HelloWorldApp {
  public static void main(String[] args) {
    // Display string
    System.out.println("Hello World!");
  }
}`,
  class_trycatch: `public class DbJob {
  void runJob() {
    try {
      System.out.println("Start");
      int x = 1 / 1;
      System.out.println(x);
    } catch (Exception e) {
      System.out.println("Error");
    }
  }
}`
};

$in.placeholder = EXAMPLES.class_hello;

$out.placeholder = `* Beispiel-Output (ABAP)
TRY.
  DELETE FROM sncars.
  INSERT sncars FROM TABLE car_tab.
  IF sy-subrc = 0.
    MESSAGE 'Car database updated successfully!' TYPE 'I'.
  ENDIF.
CATCH cx_sy_open_sql_db.
  MESSAGE 'Car database could not be updated' TYPE 'I' DISPLAY LIKE 'E'.
ENDTRY.`;

function getMode() {
  const el = document.querySelector('input[name="mode"]:checked');
  return el ? el.value : "auto";
}

function detectLooksLikeClass(text) {
  const s = (text || "").trim();
  return s.includes("class ") || s.includes("interface ") || s.includes("enum ") || s.includes("record ")
      || s.startsWith("package ") || s.startsWith("import ");
}

function setLed(state) {
  $led.classList.remove("idle","ok","err","warn");
  $led.classList.add(state);
}

function setStatus(text, state = "idle") {
  $status.textContent = text;
  setLed(state);
}

function setAutoBadge() {
  const mode = getMode();
  if (mode !== "auto") {
    $autoBadge.textContent = "Auto: —";
    return;
  }
  const kind = detectLooksLikeClass($in.value) ? "Class" : "Snippet";
  $autoBadge.textContent = `Auto: ${kind}`;
}

function modeSanityCheck(mode, text) {
  const looksClass = detectLooksLikeClass(text);
  if (mode === "class" && !looksClass) {
    return "Hinweis: „Class“ gewählt, aber dein Input sieht nach Snippet aus. Bitte „Snippet“ wählen oder eine komplette Java-Klasse einfügen.";
  }
  if (mode === "snippet" && looksClass) {
    return "Hinweis: „Snippet“ gewählt, aber dein Input sieht nach kompletter Klasse aus. Bitte „Class“ wählen oder nur Statements einfügen.";
  }
  return null;
}

function prettifyTypeName(name) {
  if (!name) return name;
  let s = String(name);
  s = s.replace(/Stmt\b/g, " statement");
  s = s.replace(/Expr\b/g, " expression");
  s = s.replace(/([a-z])([A-Z])/g, "$1 $2");
  s = s.replace(/\s+/g, " ").trim();
  return s;
}

function beautifyBackendText(s) {
  if (!s) return s;
  let out = String(s)
    .replace(/\bTODO\s+stmt\b/g, "TODO statement")
    .replace(/\bTODO\s+expr\b/g, "TODO expression");

  out = out.replace(/(TODO (?:statement|expression):\s*)([A-Za-z0-9_]+)\b/g, (m, p1, typeName) => {
    return p1 + prettifyTypeName(typeName);
  });

  return out;
}

/* ===== Line numbers ===== */
function countLines(text) {
  if (!text) return 1;
  return text.split("\n").length;
}

function updateGutter(gutterEl, textareaEl) {
  const lines = countLines(textareaEl.value || "");
  const digits = String(lines).length;
  const w = Math.max(44, 26 + digits * 10);
  document.documentElement.style.setProperty("--gutterW", `${w}px`);

  let out = "";
  for (let i = 1; i <= lines; i++) out += i + "\n";
  gutterEl.textContent = out;
  gutterEl.scrollTop = textareaEl.scrollTop;
}

function syncScroll(textarea, pre) {
  pre.scrollTop = textarea.scrollTop;
  pre.scrollLeft = textarea.scrollLeft;
}

function syncAllScroll() {
  $inGutter.scrollTop = $in.scrollTop;
  $outGutter.scrollTop = $out.scrollTop;
  syncScroll($in, $inHL.parentElement);
  syncScroll($out, $outHL.parentElement);
}

/* ===== Highlighting ===== */
function highlightJava(text) {
  $inHL.textContent = text || "";
  $inHL.className = "language-java";
  if (window.Prism && Prism.highlightElement) Prism.highlightElement($inHL);
}

function esc(s){
  return (s ?? "").replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;");
}

function tokenizeAbap(src) {
  const s = src ?? "";
  const len = s.length;
  let i = 0;
  const out = [];

  const KEYWORDS = new Set([
    "TRY","CATCH","ENDTRY",
    "CLASS","DEFINITION","IMPLEMENTATION","PUBLIC","FINAL","CREATE","SECTION",
    "METHODS","METHOD","ENDMETHOD","ENDCLASS",
    "DATA","TYPE","VALUE","IMPORTING","EXPORTING","CHANGING","RAISING",
    "IF","ELSE","ENDIF","RETURN",
    "WRITE","MESSAGE",
    "DELETE","INSERT","UPDATE","MODIFY","SELECT","FROM","TABLE","INTO","WHERE",
    "DISPLAY","LIKE"
  ]);

  function push(type, text){ if (text) out.push({type, text}); }

  while (i < len) {
    const ch = s[i];

    if ((i === 0 || s[i-1] === "\n") && ch === "*") {
      const j = s.indexOf("\n", i);
      const end = j === -1 ? len : j;
      push("comment", s.slice(i, end));
      i = end;
      continue;
    }

    if (ch === '"') {
      const j = s.indexOf("\n", i);
      const end = j === -1 ? len : j;
      push("comment", s.slice(i, end));
      i = end;
      continue;
    }

    if (ch === "'") {
      let j = i + 1;
      while (j < len && s[j] !== "'") j++;
      j = Math.min(j + 1, len);
      push("string", s.slice(i, j));
      i = j;
      continue;
    }

    if (ch >= "0" && ch <= "9") {
      let j = i + 1;
      while (j < len && /[0-9]/.test(s[j])) j++;
      push("number", s.slice(i, j));
      i = j;
      continue;
    }

    if (/[A-Za-z_]/.test(ch)) {
      let j = i + 1;
      while (j < len && /[A-Za-z0-9_\-]/.test(s[j])) j++;
      const word = s.slice(i, j);
      const upper = word.toUpperCase();
      if (KEYWORDS.has(upper) || upper === "SY-SUBRC") push("keyword", word);
      else push("plain", word);
      i = j;
      continue;
    }

    if ("=+-*/<>.,:()[]@".includes(ch)) {
      push("operator", ch);
      i++;
      continue;
    }

    push("plain", ch);
    i++;
  }

  return out;
}

function renderAbap(tokens) {
  let html = "";
  for (const t of tokens) {
    const txt = esc(t.text);
    if (t.type === "keyword") html += `<span class="token keyword">${txt}</span>`;
    else if (t.type === "string") html += `<span class="token string">${txt}</span>`;
    else if (t.type === "comment") html += `<span class="token comment">${txt}</span>`;
    else if (t.type === "number") html += `<span class="token number">${txt}</span>`;
    else if (t.type === "operator") html += `<span class="token operator">${txt}</span>`;
    else html += txt;
  }
  return html;
}

function highlightAbap(text) {
  if (!text) { $outHL.innerHTML = ""; return; }
  $outHL.innerHTML = renderAbap(tokenizeAbap(text));
}

async function parseErrorBody(res) {
  const ct = (res.headers.get("content-type") || "").toLowerCase();
  if (ct.includes("application/json")) {
    try {
      const j = await res.json();
      return j.message || j.error || JSON.stringify(j);
    } catch {
      return `HTTP ${res.status}`;
    }
  }
  return await res.text();
}

function stripNoisyStacktrace(msg) {
  if (!msg) return msg;
  const lines = String(msg).split("\n");
  return lines.length <= 10 ? msg : lines.slice(0, 10).join("\n") + "\n…";
}

/* ===== Output actions ===== */
function clearOutput() {
  $out.value = "";
  highlightAbap("");
  $copy.disabled = true;
  $download.disabled = true;
  updateGutter($outGutter, $out);
}

function enableOutput(text) {
  $copy.disabled = !text;
  $download.disabled = !text;
}

function downloadAbap() {
  const blob = new Blob([$out.value], { type: "text/plain;charset=utf-8" });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = "translation.abap";
  document.body.appendChild(a);
  a.click();
  a.remove();
  URL.revokeObjectURL(a.href);
}

function setBusy(isBusy){
  $go.disabled = isBusy;
  $loadExample.disabled = isBusy;
  $reset.disabled = isBusy;
  $examples.disabled = isBusy;
}

function friendlyNetworkMessage(e){
  const msg = (e?.message || String(e) || "").toLowerCase();
  if (msg.includes("failed to fetch") || msg.includes("networkerror")) {
    return "Netzwerk/CORS: Backend nicht erreichbar. Prüfe, ob dein Koyeb-Service läuft und CORS für chijzay.github.io erlaubt ist.";
  }
  return e.message || String(e);
}

async function doTranslate() {
  setBusy(true);
  setStatus("übersetze…", "idle");
  clearOutput();

  const mode = getMode();
  const sanity = modeSanityCheck(mode, $in.value);
  if (sanity) {
    const msg = beautifyBackendText(sanity);
    $out.value = msg;
    highlightAbap(msg);
    enableOutput(msg);
    setStatus("Hinweis", "warn");
    updateGutter($outGutter, $out);
    syncAllScroll();
    setBusy(false);
    return;
  }

  const url = `${API_BASE}/api/translate?mode=${encodeURIComponent(mode)}`;

  try {
    const res = await fetch(url, {
      method: "POST",
      headers: { "Content-Type": "text/plain" },
      body: $in.value
    });

    if (!res.ok) {
      const msg = beautifyBackendText(stripNoisyStacktrace(await parseErrorBody(res)));
      $out.value = msg;
      highlightAbap(msg);
      enableOutput(msg);
      setStatus("Fehler", "err");
      updateGutter($outGutter, $out);
      syncAllScroll();
      setBusy(false);
      return;
    }

    const textRaw = await res.text();
    const text = beautifyBackendText(textRaw);

    $out.value = text;
    highlightAbap(text);
    enableOutput(text);
    setStatus("OK", "ok");
    updateGutter($outGutter, $out);
    syncAllScroll();
  } catch (e) {
    const msg = beautifyBackendText(friendlyNetworkMessage(e));
    $out.value = msg;
    highlightAbap(msg);
    enableOutput(msg);
    setStatus("Fehler", "err");
    updateGutter($outGutter, $out);
    syncAllScroll();
  } finally {
    setBusy(false);
  }
}

/* ===== Wiring ===== */
setStatus("bereit", "idle");
setAutoBadge();

highlightJava($in.value);
highlightAbap($out.value);

updateGutter($inGutter, $in);
updateGutter($outGutter, $out);
syncAllScroll();

$in.addEventListener("input", () => {
  highlightJava($in.value);
  setAutoBadge();
  updateGutter($inGutter, $in);
});

$in.addEventListener("scroll", () => {
  updateGutter($inGutter, $in);
  syncAllScroll();
});

$out.addEventListener("scroll", () => {
  updateGutter($outGutter, $out);
  syncAllScroll();
});

document.querySelectorAll('input[name="mode"]').forEach(r => {
  r.addEventListener("change", () => setAutoBadge());
});

$go.addEventListener("click", doTranslate);

$in.addEventListener("keydown", (e) => {
  const isMac = navigator.platform.toLowerCase().includes("mac");
  const ok = (isMac && e.metaKey && e.key === "Enter") || (!isMac && e.ctrlKey && e.key === "Enter");
  if (ok) {
    e.preventDefault();
    doTranslate();
  }
});

$copy.addEventListener("click", async () => {
  await navigator.clipboard.writeText($out.value);
  setStatus("kopiert", "ok");
});

$download.addEventListener("click", () => {
  downloadAbap();
  setStatus("download", "ok");
});

$loadExample.addEventListener("click", () => {
  const key = $examples.value;
  $in.value = EXAMPLES[key] || "";
  highlightJava($in.value);
  setAutoBadge();
  updateGutter($inGutter, $in);
  setStatus("Beispiel geladen", "idle");
  syncAllScroll();
});

$reset.addEventListener("click", () => {
  $in.value = "";
  highlightJava("");
  setAutoBadge();
  updateGutter($inGutter, $in);
  clearOutput();
  setStatus("bereit", "idle");
  syncAllScroll();
});
