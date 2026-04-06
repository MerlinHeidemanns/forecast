////////////////////////////////////////////////////////////////////////////////
// Forecast Time Series Chart
//
// Consumes: /data/forecast_timeseries.json  (model posterior, daily resolution)
//           /data/polls.json                (raw poll data for scatter overlay)
//
// Design:   docs/design/step7_json_export.md
////////////////////////////////////////////////////////////////////////////////

(function () {
  "use strict";

  // ---- Layout ----
  const margin = { top: 32, right: 20, bottom: 80, left: 56 };
  const contextMargin = { top: 8, bottom: 20 };
  const contextHeight = 40;
  const minWidth = 400;

  function getChartWidth() {
    const container = document.getElementById("chart");
    if (!container) return 800;
    return Math.max(minWidth, container.clientWidth) - margin.left - margin.right;
  }

  const width = getChartWidth();
  const height = Math.min(420, Math.round(width * 0.5));

  // ---- Container ----
  const container = d3.select("#chart");
  if (container.empty()) return;

  const tooltip = container
    .append("div")
    .attr("class", "chart-tooltip")
    .style("opacity", 0)
    .style("position", "fixed")
    .style("pointer-events", "none")
    .style("background", "rgba(255,255,255,0.96)")
    .style("border", "1px solid #ddd")
    .style("border-radius", "6px")
    .style("box-shadow", "0 2px 8px rgba(0,0,0,0.12)")
    .style("padding", "10px 14px")
    .style("font-family", "inherit")
    .style("font-size", "13px")
    .style("line-height", "1.5")
    .style("z-index", "10")
    .style("max-width", "260px");

  const totalW = width + margin.left + margin.right;
  const totalH = height + margin.top + margin.bottom + contextHeight + 60;

  const svg = container
    .append("svg")
    .attr("viewBox", `0 0 ${totalW} ${totalH}`)
    .attr("width", "100%")
    .attr("preserveAspectRatio", "xMinYMin meet");

  // Clip path for main chart area
  svg.append("defs").append("clipPath")
    .attr("id", "clip-main")
    .append("rect")
    .attr("width", width)
    .attr("height", height);

  const mainChart = svg.append("g")
    .attr("class", "main-chart")
    .attr("transform", `translate(${margin.left},${margin.top})`);

  const mainClipped = mainChart.append("g")
    .attr("clip-path", "url(#clip-main)");

  const contextChart = svg.append("g")
    .attr("class", "context-chart")
    .attr("transform", `translate(${margin.left},${margin.top + height + margin.bottom - 10})`);

  // ---- Load data ----
  Promise.all([
    d3.json("/data/forecast_timeseries.json"),
    d3.json("/data/polls.json"),
  ]).then(([forecast, pollsData]) => {
    const parties = forecast.metadata.parties;        // ["CDU_CSU", "SPD", ...]
    const partyMeta = forecast.parties;               // {CDU_CSU: {display, color}, ...}
    const elections = forecast.elections;

    // Parse timeseries dates
    const parseDate = d3.timeParse("%Y-%m-%d");
    const timeseries = forecast.timeseries.map((d) => {
      const row = { date: parseDate(d.date) };
      parties.forEach((pk) => {
        row[pk] = d[pk]; // {q05, q10, q25, median, q75, q90, q95}
      });
      return row;
    });

    // Parse poll dates
    const polls = pollsData.polls.map((d) => ({
      ...d,
      date: parseDate(d.date),
    }));

    // ---- Scales ----
    const xDomain = d3.extent(timeseries, (d) => d.date);
    const x = d3.scaleTime().domain(xDomain).range([0, width]);
    const xContext = d3.scaleTime().domain(xDomain).range([0, width]);

    // y domain: max of any q95 value, with some headroom
    const yMax = d3.max(timeseries, (d) =>
      d3.max(parties, (pk) => d[pk].q95)
    );
    const y = d3.scaleLinear()
      .domain([0, Math.min(0.5, yMax * 1.08)])
      .range([height, 0]);
    const yContext = d3.scaleLinear()
      .domain(y.domain())
      .range([contextHeight, 0]);

    // ---- Grid lines ----
    mainChart.append("g")
      .attr("class", "grid-y")
      .call(
        d3.axisLeft(y)
          .ticks(6)
          .tickSize(-width)
          .tickFormat("")
      )
      .selectAll("line")
      .style("stroke", "#eee")
      .style("stroke-width", 0.5);

    mainChart.selectAll(".grid-y .domain").remove();

    // ---- Draw credible interval bands (inside clipped group) ----
    // Three nested bands per party: 90% CI (light), 80% CI (medium), 50% CI (dark)
    const bandDefs = [
      { lower: "q05", upper: "q95", opacity: 0.08, level: "90" },
      { lower: "q10", upper: "q90", opacity: 0.12, level: "80" },
      { lower: "q25", upper: "q75", opacity: 0.20, level: "50" },
    ];

    parties.forEach((pk) => {
      const color = partyMeta[pk].color;

      bandDefs.forEach((band) => {
        const area = d3.area()
          .x((d) => x(d.date))
          .y0((d) => y(d[pk][band.lower]))
          .y1((d) => y(d[pk][band.upper]))
          .curve(d3.curveLinear);

        mainClipped.append("path")
          .datum(timeseries)
          .attr("class", `band band-${pk}-${band.level}`)
          .attr("fill", color)
          .attr("opacity", band.opacity)
          .attr("d", area);
      });
    });

    // ---- Draw median lines ----
    parties.forEach((pk) => {
      const color = partyMeta[pk].color;

      const line = d3.line()
        .x((d) => x(d.date))
        .y((d) => y(d[pk].median))
        .curve(d3.curveLinear);

      mainClipped.append("path")
        .datum(timeseries)
        .attr("class", `median-line median-${pk}`)
        .attr("fill", "none")
        .attr("stroke", color)
        .attr("stroke-width", 1.8)
        .attr("d", line);
    });

    // ---- Draw poll dots (scatter overlay) ----
    const pollGroup = mainClipped.append("g").attr("class", "poll-dots");

    parties.forEach((pk) => {
      const color = partyMeta[pk].color;

      const partyPolls = polls
        .filter((d) => d[pk] != null)
        .map((d) => ({
          date: d.date,
          value: d[pk],
          pollster: d.pollster,
        }));

      pollGroup
        .selectAll(`.dot-${pk}`)
        .data(partyPolls)
        .join("circle")
        .attr("class", `dot dot-${pk}`)
        .attr("cx", (d) => x(d.date))
        .attr("cy", (d) => y(d.value))
        .attr("r", 1.5)
        .attr("fill", color)
        .attr("opacity", 0.15);
    });

    // ---- Election vertical lines ----
    elections.forEach((e) => {
      const eDate = parseDate(e.date);
      mainClipped.append("line")
        .attr("class", "election-line")
        .attr("x1", x(eDate))
        .attr("x2", x(eDate))
        .attr("y1", 0)
        .attr("y2", height)
        .attr("stroke", "#999")
        .attr("stroke-width", 0.6)
        .attr("stroke-dasharray", "3,3");
    });

    // ---- Axes ----
    const xAxis = mainChart
      .append("g")
      .attr("class", "x-axis")
      .attr("transform", `translate(0,${height})`)
      .call(d3.axisBottom(x).ticks(d3.timeYear.every(2)));

    const yAxis = mainChart
      .append("g")
      .attr("class", "y-axis")
      .call(
        d3.axisLeft(y)
          .ticks(6)
          .tickFormat(d3.format(".0%"))
      );

    // Y-axis label
    mainChart.append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", -42)
      .attr("x", -height / 2)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .style("fill", "#666")
      .text("Vote share");

    // ---- Legend (horizontal, below context chart) ----
    // Context top = margin.top + height + margin.bottom - 10
    // Context bottom = contextTop + contextHeight + 20 (axis labels)
    const legendY = margin.top + height + margin.bottom - 10 + contextHeight + 30;
    const legend = svg.append("g")
      .attr("class", "legend")
      .attr("transform", `translate(${margin.left}, ${legendY})`);

    let xOff = 0;
    parties.forEach((pk) => {
      const g = legend.append("g").attr("transform", `translate(${xOff}, 0)`);

      g.append("rect")
        .attr("width", 14)
        .attr("height", 4)
        .attr("y", -2)
        .attr("rx", 2)
        .attr("fill", partyMeta[pk].color);

      const label = g.append("text")
        .attr("x", 20)
        .attr("y", 0)
        .attr("dy", "0.35em")
        .style("font-size", "12px")
        .style("fill", "#333")
        .text(partyMeta[pk].display);

      // Measure text width and advance cursor
      const textWidth = label.node().getComputedTextLength();
      xOff += 20 + textWidth + 24;
    });

    // ---- Context chart (mini overview for brush zoom) ----
    parties.forEach((pk) => {
      const contextLine = d3.line()
        .x((d) => xContext(d.date))
        .y((d) => yContext(d[pk].median))
        .curve(d3.curveLinear);

      contextChart.append("path")
        .datum(timeseries)
        .attr("fill", "none")
        .attr("stroke", partyMeta[pk].color)
        .attr("stroke-width", 1)
        .attr("d", contextLine);
    });

    contextChart
      .append("g")
      .attr("class", "x-axis-context")
      .attr("transform", `translate(0,${contextHeight})`)
      .call(d3.axisBottom(xContext).ticks(d3.timeYear.every(4)));

    // ---- Brush ----
    const brush = d3.brushX()
      .extent([[0, 0], [width, contextHeight]])
      .on("brush end", brushed);

    contextChart.append("g")
      .attr("class", "brush")
      .call(brush)
      .call(brush.move, xContext.range());

    function brushed(event) {
      if (!event.selection) return;
      const [x0, x1] = event.selection.map(xContext.invert);
      x.domain([x0, x1]);
      updateChart();
    }

    function updateChart() {
      // Update bands
      bandDefs.forEach((band) => {
        parties.forEach((pk) => {
          const area = d3.area()
            .x((d) => x(d.date))
            .y0((d) => y(d[pk][band.lower]))
            .y1((d) => y(d[pk][band.upper]))
            .curve(d3.curveLinear);

          mainClipped.select(`.band-${pk}-${band.level}`)
            .attr("d", area(timeseries));
        });
      });

      // Update median lines
      parties.forEach((pk) => {
        const line = d3.line()
          .x((d) => x(d.date))
          .y((d) => y(d[pk].median))
          .curve(d3.curveLinear);

        mainClipped.select(`.median-${pk}`).attr("d", line(timeseries));
      });

      // Update poll dots
      parties.forEach((pk) => {
        mainClipped.selectAll(`.dot-${pk}`)
          .attr("cx", (d) => x(d.date))
          .attr("cy", (d) => y(d.value));
      });

      // Update election lines
      mainClipped.selectAll(".election-line")
        .data(elections)
        .attr("x1", (d) => x(parseDate(d.date)))
        .attr("x2", (d) => x(parseDate(d.date)));

      // Update x-axis
      xAxis.call(d3.axisBottom(x));
    }

    // ---- Tooltip on hover ----
    // Invisible overlay to track mouse position
    const bisectDate = d3.bisector((d) => d.date).left;

    mainChart.append("rect")
      .attr("class", "overlay")
      .attr("width", width)
      .attr("height", height)
      .attr("fill", "none")
      .attr("pointer-events", "all")
      .on("mousemove", function (event) {
        const [mx] = d3.pointer(event);
        const dateAtMouse = x.invert(mx);
        const idx = bisectDate(timeseries, dateAtMouse, 1);
        const d0 = timeseries[idx - 1];
        const d1 = timeseries[idx];
        if (!d0 && !d1) return;
        const d = !d1
          ? d0
          : dateAtMouse - d0.date > d1.date - dateAtMouse
          ? d1
          : d0;

        // Build tooltip content
        const dateStr = d.date.toLocaleDateString("de-DE", {
          year: "numeric",
          month: "short",
          day: "numeric",
        });

        // Sort parties by median for tooltip
        const sorted = parties
          .map((pk) => ({
            pk,
            display: partyMeta[pk].display,
            color: partyMeta[pk].color,
            median: d[pk].median,
            q05: d[pk].q05,
            q95: d[pk].q95,
          }))
          .sort((a, b) => b.median - a.median);

        let html = `<div style="font-weight:600;margin-bottom:6px;color:#555;font-size:12px">${dateStr}</div>`;
        sorted.forEach((p) => {
          const pct = (p.median * 100).toFixed(1);
          const lo = (p.q05 * 100).toFixed(1);
          const hi = (p.q95 * 100).toFixed(1);
          html += `<div style="display:flex;align-items:center;gap:6px;margin-bottom:2px">
            <span style="width:8px;height:8px;border-radius:50%;background:${p.color};flex-shrink:0"></span>
            <span style="flex:1;color:#333">${p.display}</span>
            <span style="font-weight:600;color:#111">${pct}%</span>
            <span style="color:#999;font-size:11px">(${lo}–${hi})</span>
          </div>`;
        });

        tooltip.html(html).style("opacity", 1);

        // Position with fixed coords, flip at edges
        const ttNode = tooltip.node();
        const ttW = ttNode.offsetWidth;
        const ttH = ttNode.offsetHeight;
        const vw = window.innerWidth;
        const vh = window.innerHeight;
        const cx = event.clientX;
        const cy = event.clientY;

        let left = cx + 16;
        if (left + ttW > vw - 8) left = cx - ttW - 16;

        let top = cy - 10;
        if (top + ttH > vh - 8) top = cy - ttH - 10;
        if (top < 8) top = 8;

        tooltip
          .style("left", left + "px")
          .style("top", top + "px");

        // Highlight vertical line
        mainClipped.selectAll(".hover-line").remove();
        mainClipped
          .append("line")
          .attr("class", "hover-line")
          .attr("x1", x(d.date))
          .attr("x2", x(d.date))
          .attr("y1", 0)
          .attr("y2", height)
          .attr("stroke", "#ccc")
          .attr("stroke-width", 0.8)
          .attr("pointer-events", "none");
      })
      .on("mouseleave", function () {
        tooltip.style("opacity", 0);
        mainClipped.selectAll(".hover-line").remove();
      });

    // ---- Subtitle ----
    svg.append("text")
      .attr("x", margin.left)
      .attr("y", 18)
      .style("font-size", "13px")
      .style("fill", "#888")
      .text(
        `B-spline model  ·  bands: 50% / 80% / 90% CI  ·  dots: raw polls  ·  updated ${forecast.metadata.generated_at.slice(0, 10)}`
      );

  }).catch((err) => {
    console.error("Failed to load forecast data:", err);
    container.append("p")
      .style("color", "#c00")
      .style("padding", "2rem")
      .text("Error loading forecast data. Please try again later.");
  });
})();
