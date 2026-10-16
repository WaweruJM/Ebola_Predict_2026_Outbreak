/* Ebola Outbreak Analysis Portal — front-end */

const DATA_URL = 'data/ebola_data.json';

let DATA = {
  generated_at: null,
  case_telemetry: [],
  bio_coefficients: [],
  chart_series: {},
  zones: [],
  sliders: [],
  primary_parameters: {}
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

async function loadData() {
  try {
    const res = await fetch(`${DATA_URL}?v=${Date.now()}`);
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    DATA = await res.json();
  } catch (err) {
    console.warn('Could not load ebola_data.json, using fallback.', err);
    DATA = FALLBACK_DATA;
  }
}

function renderCards(containerId, data) {
  const container = document.getElementById(containerId);
  container.innerHTML = '';
  data.forEach(item => container.appendChild(makeCard(item)));
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
             min="${s.min}" max="${s.max}" step="${s.step}" value="${s.value}">
    `;
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

// -------- Chart.js zone plugin --------
const zoneBandsPlugin = {
  id: 'zoneBands',
  beforeDatasetsDraw(chart, args, opts) {
    const { ctx, chartArea, scales } = chart;
    if (!chartArea || !opts?.zones) return;
    const x = scales.x;
    ctx.save();
    opts.zones.forEach((zone, i) => {
      const x0 = x.getPixelForValue(zone.from);
      const x1 = x.getPixelForValue(zone.to);
      const colors = [
        'rgba(74,74,74,0.85)',
        'rgba(217,217,217,0.85)',
        'rgba(255,255,255,0.92)'
      ];
      ctx.fillStyle = colors[i] || '#ffffff';
      ctx.fillRect(x0, chartArea.top, x1 - x0, chartArea.bottom - chartArea.top);
    });
    ctx.restore();
  }
};
Chart.register(zoneBandsPlugin);

// -------- Chart.js param line plugin (beta, gamma, mu) --------
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

function padTail(extraLen = 0) { return new Array(Math.max(0, extraLen || 0)).fill(null); }
function padHead(observedLen, projectedArr = []) {
  return new Array(observedLen).fill(null).concat(projectedArr);
}

function chartOptions(type) {
  return {
    responsive: true,
    maintainAspectRatio: false,
    interaction: { mode: 'index', intersect: false },
    layout: { padding: { top: 8, right: 8, bottom: 8, left: 8 } },
    plugins: {
      legend: {
        position: 'right', align: 'start',
        labels: { boxWidth: 14, boxHeight: 10, padding: 12,
                  font: { size: 11, family: 'Inter' }, color: '#1f2d3d' }
      },
      tooltip: {
        backgroundColor: 'rgba(26,82,118,0.95)',
        titleFont: { size: 12, family: 'Inter' },
        bodyFont: { size: 11, family: 'Inter' },
        padding: 10, cornerRadius: 6
      },
      zoneBands: { zones: DATA.zones }
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
        title: { display: true, text: type === 'bar' ? 'Events' : 'Count',
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
    if (b[k] && typeof b[k] === 'object' && !Array.isArray(b[k])) {
      out[k] = deepMerge(a[k] || {}, b[k]);
    } else {
      out[k] = b[k];
    }
  }
  return out;
}

function buildCharts() {
  const chartsColumn = $('#chartsColumn');
  chartsColumn.innerHTML = '';
  const S = DATA.chart_series;
  const P = DATA.primary_parameters || {};

  const defs = [
    {
      id: 'epidemicChart',
      title: 'Epidemic Curve — Suspected & Confirmed Streams',
      subtitle: 'Observed cumulative series with SIR fitted overlay',
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
      title: 'SIR Compartments with beta, gamma, mu Reference Lines',
      subtitle: 'S, I, R, D over observed and projected window',
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
      options: {
        plugins: {
          paramLines: {
            lines: [
              { value: (P.beta  || 0.4),  label: 'beta',  color: '#c0392b' },
              { value: (P.gamma || 0.15), label: 'gamma', color: '#27ae60' },
              { value: (P.mu    || 0.05), label: 'mu',    color: '#8e44ad' }
            ]
          }
        }
      }
    },
    {
      id: 'rtChart',
      title: 'Effective Reproduction Number (R-t)',
      subtitle: 'Derived from fitted SIR parameters',
      type: 'line',
      build: () => ({
        labels: S.months,
        datasets: [
          { label: 'R-t estimate', data: S.rt_series,
            borderColor: '#e67e22', backgroundColor: 'rgba(230,126,34,0.12)',
            borderWidth: 2.5, tension: 0.35, fill: true, pointRadius: 2 },
          { label: 'Threshold (R-t = 1)', data: Array(S.months.length).fill(1),
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
      </div>
    `;
    chartsColumn.appendChild(card);

    const ctx = card.querySelector(`#${def.id}`).getContext('2d');
    const merged = deepMerge(chartOptions(def.type), def.options || {});
    charts[def.id] = new Chart(ctx, {
      type: def.type,
      data: def.build(),
      options: merged
    });
  });
}

function updateCharts() {
  const beta   = parseFloat($('#slider-beta')?.value  ?? 0.42);
  const gamma  = parseFloat($('#slider-gamma')?.value ?? 0.13);
  const zoo    = parseFloat($('#slider-zoonotic')?.value ?? 12);
  const interv = parseFloat($('#slider-interv')?.value ?? 35);
  const Rt     = (beta / gamma) * (1 - interv / 100);
  const scale  = Math.max(0.4, Math.min(2.0, Rt));

  if (charts.epidemicChart) {
    ['2','3'].forEach(i => {
      const ds = charts.epidemicChart.data.datasets[i];
      ds.data = ds.data.map(v => v == null ? null : +(v * scale).toFixed(2));
    });
    charts.epidemicChart.update('none');
  }
  if (charts.sirChart) {
    const ds = charts.sirChart.data.datasets[1];
    ds.data = ds.data.map(v => v == null ? null : +(v * scale).toFixed(2));
    charts.sirChart.update('none');
  }
  if (charts.rtChart) {
    const ds = charts.rtChart.data.datasets[0];
    ds.data = ds.data.map(v => +(v * scale).toFixed(2));
    charts.rtChart.update('none');
  }
  if (charts.cumulativeChart) {
    const ds = charts.cumulativeChart.data.datasets[1];
    ds.data = ds.data.map(v => v == null ? null : Math.round(v * scale));
    charts.cumulativeChart.update('none');
  }
}

// -------- Fallback if JSON missing --------
const FALLBACK_DATA = {
  generated_at: null,
  case_telemetry: [
    { label: 'Confirmed Cases', value: 0, unit: '', delta: '--', trend: '' },
    { label: 'Suspected Cases', value: 0, unit: '', delta: '--', trend: '' },
    { label: 'Total Cases',     value: 0, unit: '', delta: '--', trend: '' },
    { label: 'Total Deaths',    value: 0, unit: '', delta: '--', trend: '' },
    { label: 'Case Fatality Rate', value: 0, unit: '%', delta: '--', trend: '' }
  ],
  bio_coefficients: [
    { label: 'beta (Transmission Rate)', value: 0, unit: '/day', delta: '' },
    { label: 'gamma (Recovery Rate)',    value: 0, unit: '/day', delta: '' },
    { label: 'mu (Mortality Rate)',      value: 0, unit: '/day', delta: '' },
    { label: 'R0 (Basic Reproduction)',  value: 0, unit: '',     delta: '' },
    { label: 'Growth Rate (r)',          value: 0, unit: '/day', delta: '' }
  ],
  chart_series: {
    months: [], suspected_observed: [], confirmed_observed: [],
    total_observed: [], deaths_observed: [], seir_fitted_I: [],
    seir_fitted_cum: [], projection_dates: [], projection_I: [],
    projection_cum: [], rt: [], rt_series: [], cumulative_confirmed: [],
    cumulative_projected: [], zoonotic_events: [],
    sir_S: [], sir_I: [], sir_R: [], sir_D: [],
    param_beta: 0, param_gamma: 0, param_mu: 0
  },
  zones: [
    { from: 0, to: 1, label: 'Animal-to-Human (Retrospective)' },
    { from: 1, to: 2, label: 'Observed Data' },
    { from: 2, to: 3, label: 'Future Prediction' }
  ],
  sliders: [
    { id: 'beta',     label: 'Transmission Rate (beta)', min: 0.05, max: 1.0, step: 0.01, value: 0.42 },
    { id: 'gamma',    label: 'Recovery Rate (gamma)',    min: 0.02, max: 0.5, step: 0.01, value: 0.13 },
    { id: 'mu',       label: 'Mortality Rate (mu)',      min: 0.0,  max: 0.5, step: 0.01, value: 0.05 },
    { id: 'zoonotic', label: 'Zoonotic Spillover',       min: 0, max: 50, step: 1, value: 12 },
    { id: 'interv',   label: 'Intervention Efficacy',    min: 0, max: 100, step: 1, value: 35 }
  ],
  primary_parameters: {}
};

async function init() {
  await loadData();
  renderCards('caseTelemetryCards', DATA.case_telemetry);
  renderCards('bioCoeffCards', DATA.bio_coefficients);
  renderSliders();
  buildCharts();
  $('#resetBtn').addEventListener('click', resetSliders);
  if (DATA.generated_at) {
    const stamp = document.createElement('span');
    stamp.style.fontSize = '0.7rem';
    stamp.textContent = `Last refresh: ${new Date(DATA.generated_at).toLocaleString()}`;
    document.querySelector('.portal-footer').appendChild(stamp);
  }
}

document.addEventListener('DOMContentLoaded', init);
