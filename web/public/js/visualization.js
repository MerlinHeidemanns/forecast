// Previous setup code remains the same until data processing
const margin = {top: 40, right: 100, bottom: 100, left: 70};
const width = 800 - margin.left - margin.right;
const height = 400 - margin.top - margin.bottom;
const contextHeight = 50;

// Enhanced color palette with opacity variations
const partyColors = {
    "CDU/CSU": {
        main: "#2C3E50",
        light: "#34495E",
        veryLight: "#ECF0F1"
    },
    "SPD": {
        main: "#E74C3C",
        light: "#F1948A",
        veryLight: "#FADBD8"
    },
    "FDP": {
        main: "#F1C40F",
        light: "#F7DC6F",
        veryLight: "#FCF3CF"
    }
};

// Set up SVG and tooltip as before...
const tooltip = d3.select("#chart")
    .append("div")
    .attr("class", "tooltip")
    .style("opacity", 0)
    .style("background", "rgba(255, 255, 255, 0.98)")
    .style("border-radius", "8px")
    .style("box-shadow", "0 4px 12px rgba(0,0,0,0.15)")
    .style("padding", "12px")
    .style("position", "absolute")
    .style("pointer-events", "none")
    .style("font-family", "'Inter', sans-serif")
    .style("transition", "all 0.2s ease");

const svg = d3.select("#chart")
    .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom + contextHeight + 40);

const mainChart = svg.append("g")
    .attr("class", "main-chart")
    .attr("transform", `translate(${margin.left},${margin.top})`);

const contextChart = svg.append("g")
    .attr("class", "context-chart")
    .attr("transform", `translate(${margin.left},${height + margin.top + 40})`);

Promise.all([
    d3.csv("/public/polling_data/Allensbach/current.csv"),
    d3.csv("/public/polling_data/Allensbach/historic.csv")
]).then(([data1, data2]) => {
    // Process and combine the data
    const combinedData = [...data1, ...data2]
        .filter(d => ["CDU/CSU", "SPD", "FDP"].includes(d.party))
        .map(d => ({
            ...d,
            publishing_date: d3.timeParse("%Y-%m-%d")(d.publishing_date),
            vote_share: +d.vote_share
        }))
        .sort((a, b) => a.publishing_date - b.publishing_date);

    // Function to calculate rolling average
    function calculateRollingAverage(data, windowSize = 3) {
        const sorted = [...data].sort((a, b) => a.publishing_date - b.publishing_date);
        return sorted.map((d, i) => {
            const start = Math.max(0, i - Math.floor(windowSize / 2));
            const end = Math.min(sorted.length, i + Math.floor(windowSize / 2) + 1);
            const window = sorted.slice(start, end);
            const avg = d3.mean(window, w => w.vote_share);
            return {
                ...d,
                vote_share_smooth: avg
            };
        });
    }

    // Process data for each party with smoothing
    const parties = Array.from(new Set(combinedData.map(d => d.party)));
    const smoothedDataByParty = {};
    parties.forEach(party => {
        const partyData = combinedData.filter(d => d.party === party);
        smoothedDataByParty[party] = calculateRollingAverage(partyData, 5); // Adjust window size as needed
    });

    // Set up scales
    const x = d3.scaleTime()
        .domain(d3.extent(combinedData, d => d.publishing_date))
        .range([0, width]);

    const y = d3.scaleLinear()
        .domain([0, d3.max(combinedData, d => d.vote_share) + 5])
        .range([height, 0]);

    // Context scales
    const xContext = d3.scaleTime()
        .domain(x.domain())
        .range([0, width]);

    const yContext = d3.scaleLinear()
        .domain(y.domain())
        .range([contextHeight, 0]);

    // Create line generators
    const line = d3.line()
        .x(d => x(d.publishing_date))
        .y(d => y(d.vote_share))
        .curve(d3.curveCatmullRom.alpha(0.5));

    const smoothLine = d3.line()
        .x(d => x(d.publishing_date))
        .y(d => y(d.vote_share_smooth))
        .curve(d3.curveCatmullRom.alpha(0.5));

    const contextLine = d3.line()
        .x(d => xContext(d.publishing_date))
        .y(d => yContext(d.vote_share_smooth))
        .curve(d3.curveCatmullRom.alpha(0.5));

    // Add grid lines
    mainChart.append("g")
        .attr("class", "grid")
        .attr("transform", `translate(0,${height})`)
        .style("stroke", "#dee2e6")
        .style("stroke-opacity", 0.5)
        .call(d3.axisBottom(x).tickSize(-height).tickFormat(""));

    mainChart.append("g")
        .attr("class", "grid")
        .style("stroke", "#dee2e6")
        .style("stroke-opacity", 0.5)
        .call(d3.axisLeft(y).tickSize(-width).tickFormat(""));

    // Draw lines for each party
    parties.forEach(party => {
        const smoothedData = smoothedDataByParty[party];

        // Draw raw data points
        mainChart.selectAll(`.point-${party.toLowerCase().replace('/', '')}`)
            .data(smoothedData)
            .join("circle")
            .attr("class", `point-${party.toLowerCase().replace('/', '')}`)
            .attr("cx", d => x(d.publishing_date))
            .attr("cy", d => y(d.vote_share))
            .attr("r", 3)
            .attr("fill", partyColors[party].main)
            .attr("opacity", 0.5)
            .style("pointer-events", "none");

        // Draw smoothed line in main chart
        mainChart.append("path")
            .datum(smoothedData)
            .attr("class", `line ${party.toLowerCase().replace('/', '')}`)
            .attr("fill", "none")
            .attr("stroke", partyColors[party].main)
            .attr("stroke-width", 3)
            .attr("d", smoothLine);

        // Draw smoothed line in context chart
        contextChart.append("path")
            .datum(smoothedData)
            .attr("fill", "none")
            .attr("stroke", partyColors[party].main)
            .attr("stroke-width", 1.5)
            .attr("d", contextLine);

        // Add interactive overlay points
        mainChart.selectAll(`.hover-point-${party.toLowerCase().replace('/', '')}`)
            .data(smoothedData)
            .join("circle")
            .attr("class", `hover-point-${party.toLowerCase().replace('/', '')}`)
            .attr("cx", d => x(d.publishing_date))
            .attr("cy", d => y(d.vote_share_smooth))
            .attr("r", 6)
            .attr("fill", "transparent")
            .attr("stroke", "transparent")
            .style("cursor", "pointer")
            .on("mouseover", function (event, d) {
                // Highlight point
                d3.select(this)
                    .transition()
                    .duration(200)
                    .attr("fill", "#fff")
                    .attr("stroke", partyColors[party].main)
                    .attr("stroke-width", 2);

                // Show tooltip
                tooltip.transition()
                    .duration(200)
                    .style("opacity", 1);

                tooltip.html(`
                    <div style="font-weight: 600; margin-bottom: 8px;">
                        ${party}
                    </div>
                    <div style="font-size: 1.2em; margin-bottom: 4px;">
                        Smoothed: ${d.vote_share_smooth.toFixed(1)}%
                    </div>
                    <div style="font-size: 0.9em; color: #666;">
                        Raw: ${d.vote_share}%
                    </div>
                    <div style="color: #666; font-size: 0.8em; margin-top: 8px;">
                        ${d.publishing_date.toLocaleDateString('de-DE', {
                    year: 'numeric',
                    month: 'long',
                    day: 'numeric'
                })}
                    </div>
                `)
                    .style("left", (event.pageX + 15) + "px")
                    .style("top", (event.pageY - 15) + "px");
            })
            .on("mouseout", function () {
                d3.select(this)
                    .transition()
                    .duration(200)
                    .attr("fill", "transparent")
                    .attr("stroke", "transparent");

                tooltip.transition()
                    .duration(500)
                    .style("opacity", 0);
            });
    });

    // Add brush
    const brush = d3.brushX()
        .extent([[0, 0], [width, contextHeight]])
        .on("brush end", brushed);

    contextChart.append("g")
        .attr("class", "brush")
        .call(brush)
        .call(brush.move, x.range());

    // Add axes
    mainChart.append("g")
        .attr("class", "x-axis")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(x));

    mainChart.append("g")
        .attr("class", "y-axis")
        .call(d3.axisLeft(y));

    contextChart.append("g")
        .attr("class", "x-axis")
        .attr("transform", `translate(0,${contextHeight})`)
        .call(d3.axisBottom(xContext));

    // Add legend
    const legend = mainChart.append("g")
        .attr("class", "legend")
        .attr("transform", `translate(${width + 20}, 10)`);

    parties.forEach((party, i) => {
        const legendGroup = legend.append("g")
            .attr("transform", `translate(0, ${i * 45})`);

        // Smoothed line
        legendGroup.append("line")
            .attr("x1", 0)
            .attr("x2", 20)
            .attr("y1", 0)
            .attr("y2", 0)
            .attr("stroke", partyColors[party].main)
            .attr("stroke-width", 3);

        // Raw data point
        legendGroup.append("circle")
            .attr("cx", 10)
            .attr("cy", 15)
            .attr("r", 3)
            .attr("fill", partyColors[party].main)
            .attr("opacity", 0.5);

        legendGroup.append("text")
            .attr("x", 30)
            .attr("y", 4)
            .style("font-size", "14px")
            .text(party);

        legendGroup.append("text")
            .attr("x", 30)
            .attr("y", 20)
            .style("font-size", "12px")
            .style("fill", "#666")
            .text("Raw data");
    });

    function brushed(event) {
        if (event.selection) {
            const [x0, x1] = event.selection.map(xContext.invert);
            x.domain([x0, x1]);

            // Update lines and points
            parties.forEach(party => {
                const smoothedData = smoothedDataByParty[party];

                mainChart.select(`.line.${party.toLowerCase().replace('/', '')}`)
                    .attr("d", smoothLine(smoothedData));

                mainChart.selectAll(`.point-${party.toLowerCase().replace('/', '')}`)
                    .attr("cx", d => x(d.publishing_date));

                mainChart.selectAll(`.hover-point-${party.toLowerCase().replace('/', '')}`)
                    .attr("cx", d => x(d.publishing_date));
            });

            // Update axes and grid
            mainChart.select(".x-axis").call(d3.axisBottom(x));
            mainChart.selectAll(".grid")
                .call(d3.axisBottom(x).tickSize(-height).tickFormat(""));
        }
    }
});