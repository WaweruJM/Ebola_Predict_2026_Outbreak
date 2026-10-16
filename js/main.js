// ---------- Chart Builder ----------
function buildCharts() {
  const chartsColumn = $('#chartsColumn');
  chartsColumn.innerHTML = '';
  const S = DATA.chart_series;
  const P = DATA.primary_parameters;

  // --- Plugin: draws β, γ, μ dotted lines on the SIR chart ---
  const paramLinesPlugin = {
    id: 'paramLines',
    afterDatasetsDraw(chart, args, opts) {
      if (!opts || !opts.lines) return;
      const { ctx, chartArea, scales } = chart;
      if (!chartArea) return;
      const y = scales.y;
      ctx.save();
      opts.lines.forEach(({ value, label, color }) => {
        const py = y.getPixelForValue(value);
        if (py < chartArea.top || py > chartArea.bottom) return;
        ctx.setLineDash([4, 4]);
        ctx.strokeStyle = color;
        ctx.lineWidth = 1.4;
        ctx.beginPath();
        ctx.moveTo(chartArea.left,  py);
        ctx.lineTo(chartArea.right, py);
        ctx.stroke();
        // Label at the right edge
        ctx.setLineDash([]);
        ctx.fillStyle = color;
        ctx.font = '600 10px Inter, sans-serif';
        ctx.textAlign = 'right';
        ctx.fillText(`${label} = ${value.toFixed(3)}`,
                     chartArea.right - 6, py - 3);
      });
      ctx.restore();
    }
  };
  Chart.register(paramLinesPlugin);

  const defs = [
    {
      id: 'epidemicChart',
      title: 'Epidemic Curve — Suspected & Confirmed Streams',
      subtitle: 'Observed cumulative series with SIR-fitted incidence overlay',
      type: 'line',
      build: () => ({
        labels: S.months.concat(S.projection_dates || []),
        datasets: [
          { label: 'Confirmed (observed)',
            data: S.confirmed_observed.concat(padTail(S.projection_dates?.length)),
            borderColor: '#e74c3c', backgroundColor: 'rgba(231,76,60,0.12)',
            borderWidth: 2.4, tension: 0.35, pointRadius: 2, fill: true },
          { label: 'Suspected (observed)',
            data: S.suspected_observed.concat(padTail(S.projection_dates?.length)),
            borderColor: '#e67e22', backgroundColor: 'rgba(230,126,34,0.10)',
            borderWidth: 2.0, tension: 0.35, pointRadius: 2 },
          { label: 'SIR fitted I(t)',
            data: S.seir_fitted_I.concat(padTail(S.projection_dates?.length)),
            borderColor: '#1a5276', borderDash: [6,4],
            borderWidth: 2.0, tension: 0.35, pointRadius: 1 },
          { label: 'SIR projection I(t)',
            data: padHead(S.months.length, S.projection_I || []),
            borderColor: '#8e44ad', borderDash: [3,3],
            borderWidth: 2.0, tension: 0.35, pointRadius: 1 }
        ]
      }),
      options: {}
    },
    {
      id: 'sirChart',
      title: 'SIR Compartment Trajectories with β, γ, μ Reference Lines',
      subtitle: 'S · I · R · D over observed and projected window',
      type: 'line',
      build: () => ({
        labels: S.months.concat(S.projection_dates || []),
        datasets: [
          { label: 'Susceptible (S)',
            data: S.sir_S.concat(padTail(S.projection_dates?.length)),
            borderColor: '#2980b9', borderWidth: 2, tension: 0.35, pointRadius: 1 },
          { label: 'Infectious (I)',
            data: S.sir_I.concat(padTail(S.projection_dates?.length)),
            borderColor: '#e74c3c', backgroundColor: 'rgba(231,76,60,0.10)',
            borderWidth: 2.4, tension: 0.35, fill: true, pointRadius: 2 },
          { label: 'Recovered (R)',
            data: S.sir_R.concat(padTail(S.projection_dates?.length)),
            borderColor: '#16a085', borderWidth: 2, tension: 0.35, pointRadius: 1 },
          { label: 'Deaths (D)',
            data: S.sir_D.concat(padTail(S.projection_dates?.length)),
            borderColor: '#7f8c8d', borderWidth: 1.6, tension: 0.35, pointRadius: 0 }
        ]
      }),
      // Values shown on a secondary y-axis so their scale (per-day rates
      // ~ 0.01–0.5) is visible against the compartment curves (100s–1e7).
      options: {
        scales: {
          y: {
            type: 'linear', position: 'left',
            title: { display: true, text: 'Individuals' }
          },
          y1: {
            type: 'linear', position: 'right',
            title: { display: true, text: 'Rate (/day)' },
            grid: { drawOnChartArea: false },
            suggestedMin: 0,
            suggestedMax: Math.max(
              (P?.beta || 0.4) * 1.2,
              (P?.gamma || 0.15) * 1.2,
              (P?.mu || 0.1) * 1.2
            )
          }
        },
        // Feeds the custom plugin: horizontal dotted lines on y1
        plugins: {
          paramLines: {
            yAxisID: 'y1',
            lines: [
              { value: P?.beta  || 0.4,   label: 'β', color: '#c0392b' },
              { value: P?.gamma || 0.15,  label: 'γ', color: '#27ae60' },
              { value: P?.mu    || 0.05,  label: 'μ', color: '#8e44ad' }
            ]
          }
        }
      }
    },
    {
      id: 'rtChart',
      title: 'Effective Reproduction Number (Rₜ)',
      subtitle: 'Derived from fitted SIR parameters (β, γ, μ)',
      type: 'line',
      build: () => ({
        labels: S.months,
        datasets: [
          { label: 'Rₜ estimate', data: S.rt_series,
            borderColor: '#e67e22', backgroundColor: 'rgba(230,126,34,0.12)',
            borderWidth: 2.5, tension: 0.35, fill: true, pointRadius: 2 },
          { label: 'Threshold (Rₜ = 1)', data: Array(S.months.length).fill(1),
            borderColor: '#c0392b', borderDash: [4,4], borderWidth: 1.5, pointRadius: 0 }
        ]
      }),
      options: {}
    },
    {
      id: 'cumulativeChart',
      title: 'Cumulative Case Trajectory — Observed vs 14-Day Projection',
      subtitle: 'Observed cumulative (green) with SIR 14-day projection (purple)',
      type: 'line',
      build: () => ({
        labels: S.months.concat(S.projection_dates || []),
        datasets: [
          { label: 'Cumulative confirmed',
            data: S.cumulative_confirmed.concat(padTail(S.projection_dates?.length)),
            borderColor: '#16a085', backgroundColor: 'rgba(22,160,133,0.14)',
            borderWidth: 2.5, tension: 0.35, fill: true, pointRadius: 2 },
          { label: 'Projected cumulative',
            data: padHead(S.months.length, S.projection_cum || []),
            borderColor: '#8e44ad', borderDash: [6,4], borderWidth: 2.5,
            tension: 0.35, pointRadius: 2 }
        ]
      }),
      options: {}
    }
  ];

  defs.forEach(def => {
    const card = document.createElement('div');
    card.className = 'chart-card';
    card.innerHTML = `
      <div class="chart-title">${def.title}</div>
      <div class="chart-subtitle">${def.subtitle}</div>
      <div class="chart-scroll">
        <div class="chart-canvas-wrap"><canvas id="${def.id}"></canvas></div>
      </div>
      <div class="chart-zone-legend">
        <span><span class="sw sw-retro"></span> Animal-to-Human (Retrospective)</span>
        <span><span class="sw sw-data"></span> Observed Data</span>
        <span><span class="sw sw-future"></span> Future Prediction</span>
        ${def.id === 'sirChart'
          ? '<span style="margin-left:12px"><b style="color:#c0392b">β</b> · <b style="color:#27ae60">γ</b> · <b style="color:#8e44ad">μ</b> dotted reference lines</span>'
          : ''}
      </div>
    `;
    chartsColumn.appendChild(card);

    const ctx = card.querySelector(`#${def.id}`).getContext('2d');
    const baseOpts = chartOptions(def.type);
    const merged   = deepMerge(baseOpts, def.options || {});
    charts[def.id] = new Chart(ctx, {
      type: def.type,
      data: def.build(),
      options: merged
    });
  });
}

// ---------- Shallow-ish merge for chart option overrides ----------
function deepMerge(a, b) {
  const out = { ...a };
  for (const k of Object.keys(b)) {
    if (b[k] && typeof b[k] === 'object' && !Array.isArray(b[k])) {
      out[k] = deepMerge(a[k] || {}, b[k]);
    } else {
      out[k] = b[k];
    }
  }
  return out;
}

// ---------- Padding helpers ----------
function padTail(extraLen = 0) { return new Array(Math.max(0, extraLen || 0)).fill(null); }
function padHead(observedLen, projectedArr = []) {
  return new Array(observedLen).fill(null).concat(projectedArr);
}
