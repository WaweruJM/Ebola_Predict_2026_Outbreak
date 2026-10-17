/* Ebola portal — revised charts, confirmed-only, train/test split */

const DATA_URL = 'data/ebola_data.json';
let DATA = {
  case_telemetry: [], bio_coefficients: [], chart_series: {},
  zones: [], sliders: [], primary_parameters: {}, performance: {}
};
let charts = {};
const $ = (sel) => document.querySelector(sel);

function makeCard({ label, value, unit = '', delta = '', trend = '' }) {
  const card = document.createElement('div');
  card.className = 'metric-card';
  card.innerHTML = `
    <div class="metric-label">${label}</div>
    <div class="metric-value">${value}<span class="metric-unit">${unit}</span></div>
    ${delta ? `<div class="metric-delta ${trend}">${delta}</div>` : ''}
  `;
  return card;
}
function renderCards(id, data) {
  const el = document.getElementById(id);
  el.innerHTML = '';
  data.forEach(d => el.appendChild(makeCard(d)));
}
function renderSliders() {
  const group = $('#sliderGroup');
  group.innerHTML = '';
  DATA.sliders.forEach(s => {
    const item = document.createElement('div');
    item.className = 'slider-item';
    item.innerHTML = `
      <label for="slider-${s.id}">
        <span>${s.label}</span>
        <span class="val" id="val-${s.id}">${s.value}</span>
      </label>
      <input type="range" id="slider-${s.id}"
             min="${s.min}" max="${s.max}" step="${s.step}" value="${s.value}">`;
    group.appendChild(item);
    const input = item.querySelector('input');
    const valEl = item.querySelector('.val');
    input.addEventListener('input', () => {
      valEl.textContent = input.value;
      updateCharts();
    });
  });
}
function resetSliders() {
  DATA.sliders.forEach(s => {
    const input = document.getElementById(`slider-${s.id}`);
    if (input) {
      input.value = s.value;
      document.getElementById(`val-${s.id}`).textContent = s.value;
    }
  });
  updateCharts();
}

// Zone plugin (train green, test amber, future white) — used ONLY on
// the predictive chart when opts.zones is provided.
const zoneBandsPlugin = {
  id: 'zoneBands',
  beforeDatasetsDraw(chart, args, opts) {
    const { ctx, chartArea, scales } = chart;
    if (!chartArea || !opts || !opts.zones) return;
    const x = scales.x;
    ctx.save();
    opts.zones.forEach(z => {
      const x0 = x.getPixelForValue(z.from);
      const x1 = x.getPixelForValue(z.to);
      ctx.fillStyle = z.color || 'rgba(255,255,255,0.4)';
      ctx.fillRect(x0, chartArea.top, x1 - x0,
                   chartArea.bottom - chartArea.top);
    });
    ctx.restore();
  }
};
Chart.register(zoneBandsPlugin);

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

function padTail(n = 0) { return new Array(Math.max(0, n || 0)).fill(null); }
function padHead(len, arr = []) { return new Array(len).fill(null).concat(arr); }

function chartOptions(type, yTitle = 'Count') {
  return {
    responsive: true, maintainAspectRatio: false,
    interaction: { mode: 'index', intersect: false },
    layout: { padding: { top: 8, right: 8, bottom: 8, left: 8 } },
    plugins: {
      legend: { position: 'right', align: 'start',
                labels: { boxWidth: 14, boxHeight: 10, padding: 12,
                          font: { size: 11, family: 'Inter' }, color: '#1f2d3d' } },
      tooltip: { backgroundColor: 'rgba(14,42,74,0.95)',
                 titleFont: { size: 12, family: 'Inter' },
                 bodyFont: { size: 11, family: 'Inter' },
                 padding: 10, cornerRadius: 6 }
    },
    scales: {
      x: {
        title: { display: true, text: 'Time (Date)',
                 font: { size: 11, weight: '600', family: 'Inter' },
                 color: '#1f2d3d', padding: { top: 6 } },
        ticks: { font: { size: 10, family: 'Inter' }, color: '#6b7c93',
                 maxRotation: 0, autoSkip: true, maxTicksLimit: 12 },
        grid: { color: 'rgba(0,0,0,0.04)' }
      },
      y: {
        beginAtZero: true,
        title: { display: true, text: yTitle,
                 font: { size: 11, weight: '600', family: 'Inter' },
                 color: '#1f2d3d' },
        ticks: { font: { size: 10, family: 'Inter' }, color: '#6b7c93' },
        grid: { color: 'rgba(0,0,0,0.06)' }
      }
    }
  };
}

function deepMerge(a, b) {
  const out = { ...a };
  for (const k of Object.keys(b)) {
    if (b[k] && typeof b[k] === 'object' && !Array.isArray(b[k]))
      out[k] = deepMerge(a[k] || {}, b[k]);
    else out[k] = b[k];
  }
  return out;
}

function buildCharts() {
  const col = $('#chartsColumn');
  col.innerHTML = '';
  const S = DATA.chart_series;
  const P = DATA.primary_parameters || {};
  const Pf = DATA.performance || {};

  const defs = [
    // 1. Confirmed-only epidemic curve (observed stream)
    {
      id: 'confirmedChart',
      title: 'Confirmed, Recovered and Confirmed-Deaths Over Time',
      subtitle: 'Observed series — no suspected cases',
      type: 'line',
      yTitle: 'Cumulative count',
      zones: null,
      build: () => ({
        labels: S.months,
        datasets: [
          { label: 'Confirmed', data: S.confirmed_observed,
            borderColor: '#e74c3c', backgroundColor: 'rgba(231,76,60,0.14)',
            borderWidth: 2.6, tension: 0.35, fill: true, pointRadius: 2 },
          { label: 'Recovered', data: S.recovered_observed,
            borderColor: '#16a085', borderWidth: 2.0, tension: 0.35, pointRadius: 2 },
          { label: 'Confirmed deaths', data: S.deaths_observed,
            borderColor: '#7f8c8d', borderWidth: 2.0, tension: 0.35, pointRadius: 1 }
        ]
      })
    },

    // 2. Compartment populations — separate y-axis from coefficients
    {
      id: 'sirPopChart',
      title: 'SIR Compartment Populations (S, I, R, D)',
      subtitle: 'Individuals — large scale, kept separate from rates',
      type: 'line',
      yTitle: 'Individuals',
      zones: null,
      build: () => ({
        labels: S.months,
        datasets: [
          { label: 'Susceptible', data: S.sir_S,
            borderColor: '#2980b9', borderWidth: 2, tension: 0.35, pointRadius: 1 },
          { label: 'Infectious', data: S.sir_I,
            borderColor: '#e74c3c', backgroundColor: 'rgba(231,76,60,0.10)',
            borderWidth: 2.4, tension: 0.35, fill: true, pointRadius: 2 },
          { label: 'Recovered', data: S.sir_R,
            borderColor: '#16a085', borderWidth: 2, tension: 0.35, pointRadius: 1 },
          { label: 'Deaths', data: S.sir_D,
            borderColor: '#7f8c8d', borderWidth: 1.6, tension: 0.35, pointRadius: 0 }
        ]
      })
    },

    // 3. Biological coefficients — small-scale y-axis, dotted lines
    {
      id: 'coeffChart',
      title: 'Biological Coefficients (beta, gamma, mu)',
      subtitle: 'Per-day rates — separate scale from compartment populations',
      type: 'line',
      yTitle: 'Rate (/day)',
      zones: null,
      build: () => ({
        labels: S.months,
        datasets: [
          { label: 'beta (transmission)', data: S.coeff_beta,
            borderColor: '#c0392b', borderWidth: 2.4, pointRadius: 0,
            tension: 0.1, borderDash: [6,4] },
          { label: 'gamma (recovery)', data: S.coeff_gamma,
            borderColor: '#27ae60', borderWidth: 2.4, pointRadius: 0,
            tension: 0.1, borderDash: [6,4] },
          { label: 'mu (mortality)', data: S.coeff_mu,
            borderColor: '#8e44ad', borderWidth: 2.4, pointRadius: 0,
            tension: 0.1, borderDash: [6,4] }
        ]
      })
    },

    // 4. Predictive chart — zones + train/test + performance box
    {
      id: 'predictiveChart',
      title: 'Predictive Model — Observed vs Fitted vs 14-Day Projection',
      subtitle: 'Zones show training (green), test (amber) and prediction (white) windows',
      type: 'line',
      yTitle: 'Cumulative confirmed cases',
      zones: true,
      showPerf: true,
      build: () => ({
        labels: S.months.concat(S.projection_dates || []),
        datasets: [
          { label: 'Observed confirmed',
            data: S.confirmed_observed.concat(padTail(S.projection_dates?.length)),
            borderColor: '#1f2d3d', borderWidth: 2.6,
            tension: 0.3, pointRadius: 2 },
          { label: 'SIR fitted',
            data: S.seir_fitted_I.concat(padTail(S.projection_dates?.length)),
            borderColor: '#e74c3c', borderWidth: 2.2,
            borderDash: [6,4], tension: 0.3, pointRadius: 1 },
          { label: 'Projection (14 d)',
            data: padHead(S.months.length, S.projection_I || []),
            borderColor: '#8e44ad', borderWidth: 2.6,
            borderDash: [3,3], tension: 0.3, pointRadius: 1 }
        ]
      })
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
      ${def.showPerf ? `
        <div class="perf-box">
          <div class="perf-cell"><span class="k">Train RMSE</span><strong>${Pf.train?.rmse?.toFixed(2) ?? '–'}</strong></div>
          <div class="perf-cell"><span class="k">Test RMSE</span><strong>${Pf.test?.rmse?.toFixed(2) ?? '–'}</strong></div>
          <div class="perf-cell"><span class="k">Overall R²</span><strong>${Pf.overall?.r2?.toFixed(3) ?? '–'}</strong></div>
          <div class="perf-cell"><span class="k">Train MAE</span><strong>${Pf.train?.mae?.toFixed(2) ?? '–'}</strong></div>
          <div class="perf-cell"><span class="k">Test MAE</span><strong>${Pf.test?.mae?.toFixed(2) ?? '–'}</strong></div>
          <div class="perf-cell"><span class="k">Overall RMSE</span><strong>${Pf.overall?.rmse?.toFixed(2) ?? '–'}</strong></div>
        </div>` : ''}
    `;
    col.appendChild(card);

    const ctx = card.querySelector(`#${def.id}`).getContext('2d');
    const opts = chartOptions(def.type, def.yTitle);
    if (def.zones) opts.plugins.zoneBands = { zones: DATA.zones };
    charts[def.id] = new Chart(ctx, {
      type: def.type,
      data: def.build(),
      options: opts
    });
  });
}

function updateCharts() {
  const beta  = parseFloat($('#slider-beta')?.value  ?? 0.42);
  const gamma = parseFloat($('#slider-gamma')?.value ?? 0.13);
  const interv= parseFloat($('#slider-interv')?.value ?? 35);
  const zoo   = parseFloat($('#slider-zoonotic')?.value ?? 12);
  const Rt    = (beta / gamma) * (1 - interv / 100);
  const scale = Math.max(0.4, Math.min(2.0, Rt));

  ['predictiveChart', 'confirmedChart'].forEach(id => {
    const c = charts[id]; if (!c) return;
    c.data.datasets.forEach((ds, i) => {
      if (i === 0) return; // keep observed unchanged
      ds.data = ds.data.map(v => v == null ? null : +(v * scale).toFixed(2));
    });
    c.update('none');
  });
  if (charts.coeffChart) {
    charts.coeffChart.data.datasets[0].data =
      charts.coeffChart.data.datasets[0].data.map(() => +beta.toFixed(3));
    charts.coeffChart.data.datasets[1].data =
      charts.coeffChart.data.datasets[1].data.map(() => +gamma.toFixed(3));
    charts.coeffChart.update('none');
  }
}

const FALLBACK_DATA = {
  case_telemetry: [
    { label: 'Confirmed Cases', value: 0, unit: '', delta: '--', trend: '' },
    { label: 'Confirmed Deaths', value: 0, unit: '', delta: '--', trend: '' },
    { label: 'Recovered', value: 0, unit: '', delta: '--', trend: '' },
    { label: 'Active Cases', value: 0, unit: '', delta: '--', trend: '' },
    { label: 'Case Fatality Rate', value: 0, unit: '%', delta: '--', trend: '' }
  ],
  bio_coefficients: [
    { label: 'beta', value: 0, unit: '/day', delta: '' },
    { label: 'gamma', value: 0, unit: '/day', delta: '' },
    { label: 'mu', value: 0, unit: '/day', delta: '' },
    { label: 'R0', value: 0, unit: '', delta: '' },
    { label: 'r', value: 0, unit: '/day', delta: '' }
  ],
  chart_series: {
    months: [], confirmed_observed: [], recovered_observed: [], deaths_observed: [],
    seir_fitted_I: [], projection_dates: [], projection_I: [], projection_cum: [],
    cumulative_confirmed: [], rt_series: [],
    sir_S: [], sir_I: [], sir_R: [], sir_D: [],
    coeff_beta: [], coeff_gamma: [], coeff_mu: []
  },
  zones: [],
  sliders: [
    { id: 'beta', label: 'Transmission Rate (beta)', min: 0.05, max: 1.0, step: 0.01, value: 0.42 },
    { id: 'gamma', label: 'Recovery Rate (gamma)', min: 0.02, max: 0.5, step: 0.01, value: 0.13 },
    { id: 'mu', label: 'Mortality Rate (mu)', min: 0.0, max: 0.5, step: 0.01, value: 0.05 },
    { id: 'zoonotic', label: 'Zoonotic Spillover', min: 0, max: 50, step: 1, value: 12 },
    { id: 'interv', label: 'Intervention Efficacy', min: 0, max: 100, step: 1, value: 35 }
  ],
  primary_parameters: {}, performance: {}
};

async function loadData() {
  try {
    const res = await fetch(`${DATA_URL}?v=${Date.now()}`);
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    DATA = await res.json();
  } catch (err) {
    console.warn('Fallback data used.', err);
    DATA = FALLBACK_DATA;
  }
}

async function init() {
  await loadData();
  renderCards('caseTelemetryCards', DATA.case_telemetry);
  renderCards('bioCoeffCards', DATA.bio_coefficients);
  renderSliders();
  buildCharts();
  $('#resetBtn').addEventListener('click', resetSliders);
}
document.addEventListener('DOMContentLoaded', init);
