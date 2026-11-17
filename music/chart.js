// One dot per play: x = minute of the local day, y = date in the year.
// Design after Sahil Chinoy's "Songs I've listened to" (https://sahilchinoy.com/music/).
// Tooltip follows Chris Henrick's convention
// (https://observablehq.com/@clhenrick/tooltip-d3-convention).
//
// Plain script, no modules: it needs the global `d3` (the v7 UMD build) loaded first.
// The year comes from the URL hash (#2025); the page redraws on hashchange.
// Data: plays/years.json and plays/<year>.csv, fetched by relative URL.
(function () {
  "use strict";

  if (typeof d3 === "undefined") {
    const stats = document.getElementById("stats");
    if (stats) stats.textContent = "The chart library (D3) did not load. Reload the page to try again.";
    return;
  }

  const DEFAULT_YEAR = 2025;
  const HEIGHT = 1800;
  const MARGIN = { top: 50, right: 20, bottom: 10, left: 30 };
  const RADIUS = 2.5;
  const MIN_WIDTH = 240; // narrowest chart drawn; phones at 400 px get 368
  const NARROW = 600; // below this width, label every 3 hours
  const TIP_GAP = 8; // px between cursor and tooltip
  const TIP_MAX = 320; // widest tooltip, px
  const TOP_N = 10; // ranks 1 to 10 each get a colour (see the CSS)

  const parseDay = d3.utcParse("%Y-%m-%d");
  const formatDay = d3.utcFormat("%b %d, %Y"); // zero-padded day: "Jun 03, 2025"
  const formatCount = d3.format(",");
  const WEEKDAY = ["Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"]; // by getUTCDay()
  const trackKey = (d) => `${d.track}\u0000${d.artist}`;
  // Draw order: unranked plays first, then rank 10 down to rank 1, so #1 is on top.
  const layerOf = (d) => (d.rank <= TOP_N ? TOP_N + 1 - d.rank : 0);
  const plural = (n, noun) => `${formatCount(n)} ${noun}${n === 1 ? "" : "s"}`;

  function hourLabel(m) {
    if (m === 0 || m === 1440) return "12 AM";
    if (m === 720) return "12 PM";
    return String((m / 60) % 12);
  }

  // "R Feb 13, 2020, 21:14"
  function formatWhen(d) {
    return `${WEEKDAY[d.date.getUTCDay()]} ${formatDay(d.date)}, ${d.time}`;
  }

  async function loadYear(year) {
    const rows = await d3.csv(`plays/${year}.csv`, (d) => ({
      track: d.track,
      artist: d.artist,
      date: parseDay(d.date),
      time: d.time,
      minutes: +d.minutes,
      n_plays: +d.n_plays,
      rank: +d.rank,
      place: (d.place || "").trim(), // empty when the column is missing or blank
    }));
    rows.sort((a, b) => layerOf(a) - layerOf(b)); // stable, so time order holds within a layer
    return rows;
  }

  function makeTooltip(tipEl, wrapEl) {
    const tip = d3.select(tipEl);

    function setContents(d) {
      tip.selectChildren().remove();
      const head = tip.append("p");
      head.append("span").attr("class", "name").text(d.track);
      head.append("span").text(` – ${d.artist}`);
      tip.append("p").attr("class", "meta").text(formatWhen(d));
      if (d.place) tip.append("p").attr("class", "meta").text(d.place);
      tip.append("p").attr("class", "meta").text(`#${d.rank} · ${plural(d.n_plays, "play")}`);
    }

    // Keep the tooltip on the side of the cursor with more room, and no wider than
    // that room, so it never pushes the page wider on a phone.
    function setPosition(event) {
      const [mx, my] = d3.pointer(event, wrapEl);
      const w = wrapEl.clientWidth;
      const h = wrapEl.clientHeight;
      const right = mx > w / 2;
      const below = my < h / 2;
      const room = (right ? mx : w - mx) - TIP_GAP;
      tip
        .style("max-width", `${Math.min(TIP_MAX, room)}px`)
        .style("left", right ? null : `${mx + TIP_GAP}px`)
        .style("right", right ? `${w - mx + TIP_GAP}px` : null)
        .style("top", below ? `${my + TIP_GAP}px` : null)
        .style("bottom", below ? null : `${h - my + TIP_GAP}px`)
        .style("text-align", right ? "right" : "left");
    }

    return {
      show(d, event) { setContents(d); setPosition(event); tip.style("display", "block"); },
      move: setPosition,
      hide() { tip.style("display", "none"); },
    };
  }

  function render(data, year, els, tooltip) {
    const width = Math.max(MIN_WIDTH, els.chart.clientWidth);

    const x = d3.scaleLinear()
      .domain([0, 1440])
      .range([MARGIN.left, width - MARGIN.right]);
    const y = d3.scaleUtc()
      .domain([Date.UTC(year, 0, 1), Date.UTC(year, 11, 31)])
      .range([MARGIN.top, HEIGHT - MARGIN.bottom]);

    tooltip.hide();
    const bar = d3.select(els.pinbar).style("display", "none");
    d3.select(els.chart).selectChildren().remove();
    const svg = d3.select(els.chart).append("svg")
      .attr("width", width)
      .attr("height", HEIGHT)
      .attr("role", "img")
      .attr("aria-label", `Every Spotify play in ${year}, by time of day and date`);

    // Weekend bands behind each Saturday and Sunday row, under the gridlines and dots.
    const rowH = y(Date.UTC(year, 0, 2)) - y(Date.UTC(year, 0, 1));
    const weekendDays = d3.utcDays(new Date(Date.UTC(year, 0, 1)), new Date(Date.UTC(year + 1, 0, 1)))
      .filter((d) => d.getUTCDay() === 6 || d.getUTCDay() === 0);
    svg.append("g")
      .attr("class", "weekends")
      .selectAll("rect")
      .data(weekendDays)
      .join("rect")
      .attr("x", MARGIN.left - RADIUS)
      .attr("width", width - MARGIN.left - MARGIN.right + 2 * RADIUS)
      .attr("y", (d) => y(d) - rowH / 2)
      .attr("height", rowH);

    const xTop = MARGIN.top - 20;
    svg.append("g")
      .attr("class", "axis x-axis")
      .attr("transform", `translate(0, ${xTop})`)
      .call(d3.axisTop(x)
        .tickValues(d3.range(0, 1441, width < NARROW ? 180 : 60))
        .tickFormat(hourLabel)
        .tickSize(-(HEIGHT - MARGIN.bottom - xTop))
        .tickPadding(4));

    svg.append("g")
      .attr("class", "axis y-axis")
      .attr("transform", `translate(${MARGIN.left}, 0)`)
      .call(d3.axisLeft(y)
        .ticks(d3.utcMonth.every(1))
        .tickFormat(d3.utcFormat("%b"))
        .tickSize(0)
        .tickPadding(6));

    const layer = svg.append("g").attr("class", "dots");
    const dots = layer.selectAll("circle")
      .data(data)
      .join("circle")
      .attr("cx", (d) => x(d.minutes))
      .attr("cy", (d) => y(d.date))
      .attr("r", RADIUS)
      .attr("data-rank", (d) => (d.rank <= TOP_N ? d.rank : null));

    // Look-ups so a hover touches only the dots it needs.
    const nodes = dots.nodes();
    const datum = (n) => d3.select(n).datum();
    const byArtist = d3.group(nodes, (n) => datum(n).artist);
    const byTrack = d3.group(nodes, (n) => trackKey(datum(n)));
    const byRank = d3.group(nodes.filter((n) => datum(n).rank <= TOP_N), (n) => datum(n).rank);

    function raiseRanked() {
      for (let r = TOP_N; r >= 1; r--) {
        const group = byRank.get(r);
        if (group) d3.selectAll(group).raise();
      }
    }

    let active = null; // the highlighted dot (hovered, or pinned)
    let pinned = null; // the pinned dot, if any
    let pendingClear = null;

    function highlight(node) {
      const d = datum(node);
      svg.classed("hovering", true);
      d3.selectAll(byArtist.get(d.artist)).classed("same-artist", true).raise();
      d3.selectAll(byTrack.get(trackKey(d))).classed("same-track", true).raise();
    }

    function clear() {
      if (!active) return;
      const d = datum(active);
      svg.classed("hovering", false);
      d3.selectAll(byArtist.get(d.artist)).classed("same-artist", false);
      d3.selectAll(byTrack.get(trackKey(d))).classed("same-track", false);
      raiseRanked();
      active = null;
    }

    // Click a dot to pin its artist and track highlight. Clicking the same track again,
    // clicking empty chart space, or pressing Esc clears it. While a dot is pinned,
    // hovering other dots shows only their tooltip.
    function pin(node) {
      if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
      clear();
      active = node;
      pinned = node;
      highlight(node);
      const d = datum(node);
      bar.selectChildren().remove();
      bar.append("span").attr("class", "name").text(d.track);
      bar.append("span").text(` – ${d.artist} · ${plural(d.n_plays, "play")} this year · `);
      bar.append("span").attr("class", "hint").text("click again or Esc to clear");
      bar.style("display", "block");
    }

    function unpin() {
      pinned = null;
      clear();
      tooltip.hide();
      bar.style("display", "none");
    }

    // Raising a dot moves it in the DOM, which can fire mouseout and mouseover on the
    // same dot. Clearing on the next frame lets a repeat mouseover cancel the clear.
    layer
      .on("mouseover", (event) => {
        const node = event.target;
        if (node.tagName !== "circle") return;
        if (pinned) { tooltip.show(datum(node), event); return; }
        if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
        if (node === active) return;
        clear();
        active = node;
        highlight(node);
        tooltip.show(datum(node), event);
      })
      .on("mousemove", (event) => { if (active) tooltip.move(event); })
      .on("mouseout", (event) => {
        if (event.target.tagName !== "circle") return;
        if (pinned) { tooltip.hide(); return; }
        pendingClear = requestAnimationFrame(() => {
          pendingClear = null;
          clear();
          tooltip.hide();
        });
      })
      .on("click", (event) => {
        const node = event.target;
        if (node.tagName !== "circle") return;
        event.stopPropagation();
        const d = datum(node);
        if (pinned && trackKey(datum(pinned)) === trackKey(d)) unpin();
        else { pin(node); tooltip.show(d, event); }
      });
    svg.on("click", () => { if (pinned) unpin(); });
    d3.select(window).on("keydown.pin", (event) => {
      if (event.key === "Escape" && pinned) unpin();
    });
  }

  async function boot() {
    const els = {
      chart: document.getElementById("chart"),
      wrap: document.getElementById("chart-wrap"),
      tooltip: document.getElementById("tooltip"),
      pinbar: document.getElementById("pinbar"),
      years: document.getElementById("years"),
      stats: document.getElementById("stats"),
    };
    const tooltip = makeTooltip(els.tooltip, els.wrap);

    const years = (await d3.json("plays/years.json")).map((d) => +d.year).sort((a, b) => b - a);
    const fallback = years.includes(DEFAULT_YEAR) ? DEFAULT_YEAR : years[0];
    const yearFromHash = () => {
      const asked = +window.location.hash.slice(1);
      return years.includes(asked) ? asked : fallback;
    };

    const links = d3.select(els.years)
      .selectAll("span.year")
      .data(years)
      .join("span")
      .attr("class", "year");
    links.append("a")
      .attr("href", (d) => `#${d}`)
      .text((d) => d);
    links.filter((d, i, nodes) => i < nodes.length - 1)
      .append("span")
      .attr("class", "sep")
      .text(" / ");

    let current = null; // { year, data, drawnFor }: what is drawn now
    let wanted = null; // the year most recently asked for
    let ticket = 0; // ignores a slow load once a newer year has been asked for

    async function show(year) {
      if (year === wanted) return;
      wanted = year;
      document.title = `${year} in Music`;
      const mine = ++ticket;
      links.select("a")
        .classed("active", (d) => d === year)
        .attr("aria-current", (d) => (d === year ? "page" : null));
      let data;
      try {
        data = await loadYear(year);
      } catch (err) {
        if (mine === ticket) {
          wanted = null; // let the same link retry
          els.stats.textContent = `The plays for ${year} did not load. Reload the page to try again.`;
        }
        return;
      }
      if (mine !== ticket) return;
      const nTracks = new Set(data.map(trackKey)).size;
      const nArtists = new Set(data.map((d) => d.artist)).size;
      els.stats.textContent =
        `${plural(data.length, "play")} · ${plural(nTracks, "track")} · ${plural(nArtists, "artist")}`;
      render(data, year, els, tooltip);
      current = { year, data, drawnFor: els.chart.clientWidth };
    }

    window.addEventListener("hashchange", () => show(yearFromHash()));

    let timer = null;
    window.addEventListener("resize", () => {
      clearTimeout(timer);
      timer = setTimeout(() => {
        if (current && els.chart.clientWidth !== current.drawnFor) {
          render(current.data, current.year, els, tooltip);
          current.drawnFor = els.chart.clientWidth;
        }
      }, 150);
    });

    await show(yearFromHash());
  }

  function start() {
    boot().catch(() => {
      const stats = document.getElementById("stats");
      if (stats) stats.textContent = "The list of years did not load. Reload the page to try again.";
    });
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else start();
})();
