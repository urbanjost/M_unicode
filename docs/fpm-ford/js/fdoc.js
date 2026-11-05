(function () {
  var root = document.documentElement;
  var saved = null;
  try { saved = localStorage.getItem("fdoc-theme"); } catch (e) {}
  if (saved) root.setAttribute("data-theme", saved);
  document.addEventListener("DOMContentLoaded", function () {
    var button = document.querySelector(".theme");
    if (button) button.addEventListener("click", function () {
      var dark = root.getAttribute("data-theme") ? root.getAttribute("data-theme") === "dark"
        : window.matchMedia("(prefers-color-scheme: dark)").matches;
      var next = dark ? "light" : "dark";
      root.setAttribute("data-theme", next);
      try { localStorage.setItem("fdoc-theme", next); } catch (e) {}
    });
    document.querySelectorAll(".codehilite").forEach(function (block) {
      if (!navigator.clipboard) return;
      var copy = document.createElement("button");
      copy.className = "copy";
      copy.textContent = "Copy";
      copy.addEventListener("click", function () {
        navigator.clipboard.writeText(block.querySelector("pre").innerText).then(function () {
          copy.textContent = "Copied";
          setTimeout(function () { copy.textContent = "Copy"; }, 1200);
        });
      });
      block.appendChild(copy);
    });
  });
})();
(function () {
  var script = document.currentScript;
  var root = script ? script.src.replace(/js\/fdoc\.js.*$/, "") : "";
  var loaded = false, entries = null, waiting = [];

  function load(then) {
    if (entries) { then(); return; }
    waiting.push(then);
    if (loaded) return;
    loaded = true;
    var tag = document.createElement("script");
    tag.src = root + "search/index.js";
    tag.onload = function () {
      entries = window.FDOC_INDEX || [];
      entries.forEach(function (e) { e.low = e.t.toLowerCase(); e.text = (e.s || "").toLowerCase(); });
      waiting.forEach(function (f) { f(); });
    };
    document.head.appendChild(tag);
  }

  function score(entry, words) {
    var total = 0;
    for (var i = 0; i < words.length; i++) {
      var w = words[i], at = entry.low.indexOf(w);
      if (entry.low === w) total += 100;
      else if (at === 0) total += 50;
      else if (at > 0) total += 25;
      else if (entry.text.indexOf(w) >= 0) total += 5;
      else return 0;
    }
    return total - entry.low.length / 100;
  }

  // edit distance with transpositions, giving up once it exceeds `most`
  function distance(a, b, most) {
    if (Math.abs(a.length - b.length) > most) return most + 1;
    var before = null, last = [], row = [], i, j;
    for (j = 0; j <= b.length; j++) last[j] = j;
    for (i = 1; i <= a.length; i++) {
      row = [i];
      var best = i;
      for (j = 1; j <= b.length; j++) {
        var cost = a[i - 1] === b[j - 1] ? 0 : 1;
        row[j] = Math.min(last[j] + 1, row[j - 1] + 1, last[j - 1] + cost);
        if (before && i > 1 && j > 1 && a[i - 1] === b[j - 2] && a[i - 2] === b[j - 1]) {
          row[j] = Math.min(row[j], before[j - 2] + 1);
        }
        best = Math.min(best, row[j]);
      }
      if (best > most) return most + 1;
      before = last;
      last = row;
    }
    return last[b.length];
  }

  function parts(text) { return text.split(/[^a-z0-9]+/).filter(Boolean); }

  // a typo: the word is close to some run of parts of the name, or to how such a run starts
  function typo(tokens, word) {
    if (word.length < 3) return 0;
    var size = parts(word).length, plain = parts(word).join("_"), most = word.length < 8 ? 1 : 2;
    var result = 0;
    for (var i = 0; i + size <= tokens.length; i++) {
      var run = tokens.slice(i, i + size).join("_");
      var d = distance(plain, run, most);
      if (d > most) d = distance(plain, run.slice(0, plain.length), most);
      if (d <= most) result = Math.max(result, 3 - d);
    }
    return result;
  }

  // the word is in the initials of the parts of the name, as `jvcs` for json_value_create_string
  function initials(tokens, word) {
    if (word.length < 2) return 0;
    return subsequence(tokens.map(function (t) { return t[0]; }).join(""), word) ? 2.5 : 0;
  }

  function subsequence(text, word) {
    var at = -1;
    for (var i = 0; i < word.length; i++) {
      at = text.indexOf(word[i], at + 1);
      if (at < 0) return false;
    }
    return true;
  }

  // the letters of the word are in order and close together
  function letters(low, word) {
    if (word.length < 3) return 0;
    var best = 0;
    for (var start = low.indexOf(word[0]); start >= 0; start = low.indexOf(word[0], start + 1)) {
      var at = start;
      for (var i = 1; i < word.length && at >= 0; i++) at = low.indexOf(word[i], at + 1);
      if (at < 0) break;
      var span = at - start + 1;
      if (span <= word.length * 2) best = Math.max(best, word.length / span);
    }
    return best;
  }

  function fuzzy(entry, words) {
    if (!entry.tokens) entry.tokens = parts(entry.low);
    var total = 0;
    for (var i = 0; i < words.length; i++) {
      var w = words[i];
      var s = entry.low.indexOf(w) >= 0 ? 3 : Math.max(typo(entry.tokens, w), initials(entry.tokens, w), letters(entry.low, w));
      if (s <= 0) return 0;
      total += s;
    }
    return total - entry.low.length / 1000;
  }

  function search(query, limit) {
    var words = query.toLowerCase().split(/\s+/).filter(Boolean);
    if (!words.length) return [];
    var found = [], rest = [];
    entries.forEach(function (e) {
      var s = score(e, words);
      if (s > 0) found.push([s, e]); else rest.push(e);
    });
    found.sort(function (a, b) { return b[0] - a[0]; });
    if (found.length < limit) {
      // fuzzy matches only fill in below exact ones
      var near = [];
      rest.forEach(function (e) { var s = fuzzy(e, words); if (s > 0) near.push([s, e]); });
      near.sort(function (a, b) { return b[0] - a[0]; });
      found = found.concat(near);
    }
    return found.slice(0, limit).map(function (f) { return f[1]; });
  }

  function row(entry) {
    var a = document.createElement("a");
    a.href = root + entry.u;
    var title = document.createElement("b");
    title.textContent = entry.t;
    var kind = document.createElement("span");
    kind.className = "badge";
    kind.textContent = entry.k;
    a.appendChild(title);
    a.appendChild(kind);
    if (entry.s) {
      var text = document.createElement("small");
      text.textContent = entry.s;
      a.appendChild(text);
    }
    return a;
  }

  function fill(box, results) {
    box.textContent = "";
    results.forEach(function (e) { box.appendChild(row(e)); });
  }

  document.addEventListener("DOMContentLoaded", function () {
    var input = document.querySelector(".search input");
    if (!input) return;
    var box = document.querySelector(".search .results");
    var selected = -1;
    function mark() {
      var rows = box.children;
      for (var i = 0; i < rows.length; i++) rows[i].className = i === selected ? "on" : "";
    }
    input.addEventListener("input", function () {
      load(function () {
        fill(box, input.value ? search(input.value, 12) : []);
        selected = -1;
        box.style.display = box.children.length ? "block" : "none";
      });
    });
    input.addEventListener("keydown", function (event) {
      var n = box.children.length;
      if (event.key === "ArrowDown" && n) { selected = (selected + 1) % n; mark(); event.preventDefault(); }
      else if (event.key === "ArrowUp" && n) { selected = (selected + n - 1) % n; mark(); event.preventDefault(); }
      else if (event.key === "Escape") { box.style.display = "none"; input.blur(); }
      else if (event.key === "Enter") {
        if (selected >= 0) box.children[selected].click();
        else location.href = root + "search.html?q=" + encodeURIComponent(input.value);
      }
    });
    document.addEventListener("click", function (event) {
      if (!event.target.closest(".search")) box.style.display = "none";
    });
    document.addEventListener("keydown", function (event) {
      if (event.key === "/" && document.activeElement !== input) { input.focus(); event.preventDefault(); }
    });
    var page = document.querySelector(".search-page");
    if (page) {
      var query = new URLSearchParams(location.search).get("q") || "";
      input.value = query;
      load(function () {
        var results = search(query, 200);
        fill(page, results);
        document.querySelector(".search-count").textContent = results.length + " results for “" + query + "”";
      });
    }
  });
})();
(function () {
  document.addEventListener("DOMContentLoaded", function () {
    document.querySelectorAll(".graph").forEach(function (box) {
      var svg = box.querySelector("svg");
      if (!svg) return;
      var width = svg.width.baseVal.value, height = svg.height.baseVal.value, scale = 1;
      function set(value) {
        scale = Math.max(0.25, Math.min(6, value));
        box.classList.add("zoomed");
        svg.style.width = width * scale + "px";
        svg.style.height = height * scale + "px";
      }
      var zoom = document.createElement("div");
      zoom.className = "zoom";
      [["+", 1.3], ["\u2212", 1 / 1.3]].forEach(function (b) {
        var button = document.createElement("button");
        button.textContent = b[0];
        button.setAttribute("aria-label", b[1] > 1 ? "Zoom in" : "Zoom out");
        button.addEventListener("click", function () { set(scale * b[1]); });
        zoom.appendChild(button);
      });
      box.insertBefore(zoom, svg);
      var fit = box.clientWidth / width;
      if (fit > 0 && fit < 0.75) set(0.75);
      var start = null;
      box.addEventListener("mousedown", function (event) {
        if (event.target.closest("a, button")) return;
        start = [event.clientX, event.clientY, box.scrollLeft, box.scrollTop];
        box.classList.add("dragging");
      });
      window.addEventListener("mousemove", function (event) {
        if (!start) return;
        box.scrollLeft = start[2] - (event.clientX - start[0]);
        box.scrollTop = start[3] - (event.clientY - start[1]);
      });
      window.addEventListener("mouseup", function () { start = null; box.classList.remove("dragging"); });
    });
  });
})();

// a link to something inside a closed section opens the section
function openTarget() {
  var id = decodeURIComponent(location.hash.slice(1));
  var target = id && document.getElementById(id);
  var section = target && target.closest('details');
  if (section) { section.open = true; target.scrollIntoView(); }
}
window.addEventListener('hashchange', openTarget);
window.addEventListener('DOMContentLoaded', openTarget);
