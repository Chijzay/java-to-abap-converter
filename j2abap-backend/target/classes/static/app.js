const API_BASE = "";

const $in = document.getElementById("in");
const $out = document.getElementById("out");
const $go = document.getElementById("go");
const $copy = document.getElementById("copy");
const $status = document.getElementById("status");

const API_BASE = (location.port === "8080") ? "" : "http://localhost:8080";

function getMode() {
  const el = document.querySelector('input[name="mode"]:checked');
  return el ? el.value : "auto";
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

$go.onclick = async () => {
  $status.textContent = "…";
  $status.classList.remove("ok", "err");
  $copy.disabled = true;
  $out.value = "";

  const mode = getMode();
  const url = `${API_BASE}/api/translate?mode=${encodeURIComponent(mode)}`;

  try {
    const res = await fetch(url, {
      method: "POST",
      headers: { "Content-Type": "text/plain" }, // wichtig: simpel halten
      body: $in.value
    });

    if (!res.ok) {
      const msg = await parseErrorBody(res);
      throw new Error(msg || `HTTP ${res.status}`);
    }

    $out.value = await res.text();
    $copy.disabled = !$out.value;
    $status.textContent = "OK";
    $status.classList.add("ok");
  } catch (e) {
    $status.textContent = `Fehler: ${e.message}`;
    $status.classList.add("err");
  }
};

$copy.onclick = async () => {
  await navigator.clipboard.writeText($out.value);
  $status.textContent = "kopiert";
  $status.classList.add("ok");
};
