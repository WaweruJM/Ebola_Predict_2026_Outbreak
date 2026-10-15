/* ============================================================
   Ebola Outbreak Analysis Portal — Main Script
   ============================================================ */

// ---------- Configuration ----------
const API_BASE_URL = '/api'; // change to your backend URL

// ---------- Reference Data ----------
const CASE_TELEMETRY = [
    { label: 'Confirmed Cases',     value: 1284, unit: '',   delta: '+42 today', trend: 'up' },
    { label: 'Suspected Cases',     value: 476,  unit: '',   delta: '+11 today', trend: 'up' },
    { label: 'Total Deaths',        value: 892,  unit: '',   delta: '+9 today',  trend: 'up' },
    { label: 'Recovered',           value: 358,  unit: '',   delta: '+28 today', trend: 'down' },
    { label: 'Active Cases',        value: 510,  unit: '',   delta: '-4 today',  trend: 'down' },
    { label: 'Case Fatality Rate',  value: 69.5, unit: '%',  delta: '+0.3 pts',  trend: 'up' },
    { label: 'Affected Districts',  value: 23,   unit: '',   delta: '+1 today',  trend: 'up' },
    { label: 'Health Workers Infected', value: 47, unit: '', delta: '+2 today',  trend: 'up' }
];

const BIO_COEFFICIENTS = [
    { label: 'R₀ (Basic Reproduction)', value: 1.85, unit: '',  delta: '95% CI: 1.62–2.08' },
    { label: 'Rₜ (Effective Repro.)',   value: 1.24, unit: '',  delta: '95% CI: 1.08–1.41' },
    { label: 'Incubation Period',       value: 9.4,  unit: 'd', delta: 'Mean, IQR 7–12 d' },
    { label: 'Infectious Period',       value: 7.8,  unit: 'd', delta: 'Mean, IQR 5–10 d' },
    { label: 'Serial Interval',         value: 12.1, unit: 'd', delta: 'Mean, SD 4.3 d' },
    { label: 'CFR (Modelled)',          value: 68.2, unit: '%', delta: 'Range 62–74%' },
    { label: 'Doubling Time',           value: 14.6, unit: 'd', delta: 'Early phase' },
    { label: 'Generation Time',         value: 15.3, unit: 'd', delta: 'Mean estimate' }
];

// Slider definitions
const SLIDERS = [
    { id: 'beta',    label: 'Transmission Rate (β)', min: 0.1, max: 1.0,  step: 0.01, value: 0.42 },
    { id: 'gamma',   label: 'Recovery Rate (γ)',     min: 0.05, max: 0.5, step: 0.01, value: 0.13 },
    { id: 'mu',      label: 'Mortality Rate (μ)',    min: 0.0,  max: 1.0, step: 0.01, value: 0.68 },
    { id: 'zoonotic',label: 'Zoonotic Spillover',    min: 0,    max: 50,  step: 1,    value: 12 },
    { id: 'interv',  label: 'Intervention Efficacy', min: 0,    max: 100, step: 1,    value: 35 }
];

// ---------- Utilities ----------
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

// ---------- Render Metric Rows ----------
function renderCards(containerId, data) {
    const container = document.getElementById(containerId);
    container.innerHTML = '';
    data.forEach(item => container.appendChild(makeCard(item)));
}

// ---------- Render Sliders ----------
function renderSliders() {
    const group = $('#sliderGroup');
    group.innerHTML = '';
    SLIDERS.forEach(s => {
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
    SLIDERS.forEach(s => {
        const input = document.getElementById(`slider-${s.id}`);
        if (input) {
            input.value = s.value;
            document.getElementById(`val-${s.id}`).textContent = s.value;
        }
    });
    updateCharts();
}

// ---------- Chart Zone Plugin ----------
// Draws vertical shaded bands for retrospective (dark grey),
// observed data (light grey) and future prediction (white).
const zoneBandsPlugin = {
    id: 'zoneBands',
    beforeDatasetsDraw(chart, args, opts) {
        const { ctx, chartArea, scales } = chart;
        if (!chartArea || !opts?.zones) return;
        const x = scales.x;
        ctx.save();
        opts.zones.forEach(zone => {
            const x0 = x.getPixelForValue(zone.from);
            const x1 = x.getPixelForValue(zone.to);
            ctx.fillStyle = zone.color;
            ctx.fillRect(x0, chartArea.top, x1 - x0, chartArea.bottom - chartArea.top);
        });
        ctx.restore();
    }
};
Chart.register(zoneBandsPlugin);

// ---------- Chart Definitions ----------
const MONTHS = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec',
                'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];

const ZONES = [
    { from: 0,  to: 4,  color: 'rgba(74,74,74,0.85)' },   // retrospective / zoonotic
    { from: 4,  to: 12, color: 'rgba(217,217,217,0.85)' }, // observed data
    { from: 12, to: 23, color: 'rgba(255,255,255,0.9)' }   // future prediction
];

const charts = {};

function buildCharts() {
    const chartsColumn = $('#chartsColumn');
    chartsColumn.innerHTML = '';

    const definitions = [
        {
            id: 'casesChart',
            title: 'Reported Cases & Deaths Over Time',
            subtitle: 'Observed and projected epidemic curve (Jan 2025 – Dec 2026)',
            type: 'line',
            data: makeCasesData()
        },
        {
            id: 'rtChart',
            title: 'Effective Reproduction Number (Rₜ)',
            subtitle: 'Time-varying Rₜ with 95% credible intervals',
            type: 'line',
            data: makeRtData()
        },
        {
            id: 'zoonoticChart',
            title: 'Zoonotic Spillover Events',
            subtitle: 'Estimated animal-to-human transmission events per month',
            type: 'bar',
            data: makeZoonoticData()
        },
        {
            id: 'cumulativeChart',
            title: 'Cumulative Case Trajectory',
            subtitle: 'Cumulative confirmed cases under current intervention scenario',
            type: 'line',
            data: makeCumulativeData()
        }
    ];

    definitions.forEach(def => {
        const card = document.createElement('div');
        card.className = 'chart-card';
        card.innerHTML = `
            <div class="chart-title">${def.title}</div>
            <div class="chart-subtitle">${def.subtitle}</div>
            <div class="chart-scroll">
                <div class="chart-canvas-wrap">
                    <canvas id="${def.id}"></canvas>
                </div>
            </div>
            <div class="chart-zone-legend">
                <span><span class="sw sw-retro"></span> Animal-to-Human (Retrospective)</span>
                <span><span class="sw sw-data"></span> Observed Data</span>
                <span><span class="sw sw-future"></span> Future Prediction</span>
            </div>
        `;
        chartsColumn.appendChild(card);

        const ctx = card.querySelector(`#${def.id}`).getContext('2d');
        charts[def.id] = new Chart(ctx, {
            type: def.type,
            data: def.data,
            options: chartOptions(def.type)
        });
    });
}

// ---------- Data Generators (mock — replace with API) ----------
function makeCasesData() {
    const observed = [2,5,9,18,34,62,105,168,242,318,392,455,510,548,570];
    const future   = Array(9).fill(null);
    const projected = Array(14).fill(null).concat([570,612,648,672,685,690,688,680,668]);
    return {
        labels: MONTHS.slice(0, 24),
        datasets: [
            {
                label: 'Reported Cases',
                data: observed.concat(Array(24 - observed.length).fill(null)),
                borderColor: '#e74c3c',
                backgroundColor: 'rgba(231,76,60,0.12)',
                borderWidth: 2.5,
                tension: 0.35,
                fill: true,
                pointRadius: 3
            },
            {
                label: 'Projected Cases',
                data: projected,
                borderColor: '#1a5276',
                borderDash: [6,4],
                backgroundColor: 'rgba(26,82,118,0.10)',
                borderWidth: 2.5,
                tension: 0.35,
                fill: true,
                pointRadius: 3
            },
            {
                label: 'Deaths',
                data: [1,3,6,12,24,44,74,118,170,224,276,320,358,384,398].concat(Array(9).fill(null)),
                borderColor: '#7f8c8d',
                backgroundColor: 'rgba(127,140,141,0.08)',
                borderWidth: 2,
                tension: 0.35,
                pointRadius: 2
            }
        ]
    };
}

function makeRtData() {
    const vals = [2.4,2.3,2.2,2.1,1.95,1.85,1.78,1.72,1.66,1.58,1.5,1.42,1.35,1.28,1.22,1.18,1.15,1.12,1.10,1.09,1.08,1.07,1.06,1.05];
    return {
        labels: MONTHS.slice(0, 24),
        datasets: [
            {
                label: 'Rₜ estimate',
                data: vals,
                borderColor: '#e67e22',
                backgroundColor: 'rgba(230,126,34,0.12)',
                borderWidth: 2.5,
                tension: 0.35,
                fill: true,
                pointRadius: 3
            },
            {
                label: 'Threshold (Rₜ = 1)',
                data: Array(24).fill(1),
                borderColor: '#c0392b',
                borderDash: [4,4],
                borderWidth: 1.5,
                pointRadius: 0
            }
        ]
    };
}

function makeZoonoticData() {
    return {
        labels: MONTHS.slice(0, 15),
        datasets: [
            {
                label: 'Spillover events',
                data: [4,6,8,10,12,11,9,7,6,5,4,3,3,2,2],
                backgroundColor: 'rgba(74,74,74,0.75)',
                borderColor: '#2c3e50',
                borderWidth: 1,
                borderRadius: 4
            }
        ]
    };
}

function makeCumulativeData() {
    const obs = [2,7,16,34,68,130,235,403,645,963,1355,1810,2320,2868,3438];
    return {
        labels: MONTHS.slice(0, 24),
        datasets: [
            {
                label: 'Cumulative confirmed',
                data: obs.concat(Array(24 - obs.length).fill(null)),
                borderColor: '#16a085',
                backgroundColor: 'rgba(22,160,133,0.14)',
                borderWidth: 2.5,
                tension: 0.35,
                fill: true,
                pointRadius: 3
            },
            {
                label: 'Projected cumulative',
                data: Array(14).fill(null).concat([3438,4050,4698,5370,6055,6745,7433,8101,8731]),
                borderColor: '#8e44ad',
                borderDash: [6,4],
                borderWidth: 2.5,
                tension: 0.35,
                pointRadius: 3
            }
        ]
    };
}

// ---------- Chart Options (clean, non-overlapping) ----------
function chartOptions(type) {
    return {
        responsive: true,
        maintainAspectRatio: false,
        interaction: { mode: 'index', intersect: false },
        layout: { padding: { top: 8, right: 8, bottom: 8, left: 8 } },
        plugins: {
            legend: {
                position: 'right',
                align: 'start',
                labels: {
                    boxWidth: 14,
                    boxHeight: 10,
                    padding: 12,
                    font: { size: 11, family: 'Inter' },
                    color: '#1f2d3d'
                }
            },
            tooltip: {
                backgroundColor: 'rgba(26,82,118,0.95)',
                titleFont: { size: 12, family: 'Inter' },
                bodyFont: { size: 11, family: 'Inter' },
                padding: 10,
                cornerRadius: 6
            },
            zoneBands: { zones: ZONES }
        },
        scales: {
            x: {
                title: {
                    display: true,
                    text: 'Time (Month)',
                    font: { size: 11, weight: '600', family: 'Inter' },
                    color: '#1f2d3d',
                    padding: { top: 6 }
                },
                ticks: {
                    font: { size: 10, family: 'Inter' },
                    color: '#6b7c93',
                    maxRotation: 0,
                    autoSkip: true,
                    maxTicksLimit: 12
                },
                grid: { color: 'rgba(0,0,0,0.04)' }
            },
            y: {
                beginAtZero: true,
                title: {
                    display: true,
                    text: type === 'bar' ? 'Events' : 'Count / Value',
                    font: { size: 11, weight: '600', family: 'Inter' },
                    color: '#1f2d3d'
                },
                ticks: {
                    font: { size: 10, family: 'Inter' },
                    color: '#6b7c93'
                },
                grid: { color: 'rgba(0,0,0,0.06)' }
            }
        }
    };
}

// ---------- Update charts on slider change ----------
function updateCharts() {
    const beta    = parseFloat($('#slider-beta')?.value ?? 0.42);
    const gamma   = parseFloat($('#slider-gamma')?.value ?? 0.13);
    const mu      = parseFloat($('#slider-mu')?.value ?? 0.68);
    const zoo     = parseFloat($('#slider-zoonotic')?.value ?? 12);
    const interv  = parseFloat($('#slider-interv')?.value ?? 35);

    // Simple SIR-like scaling for demonstration
    const Rt = (beta / gamma) * (1 - interv / 100);
    const scale = Math.max(0.4, Math.min(2.0, Rt));

    // Scale cases chart projected dataset
    const casesChart = charts['casesChart'];
    if (casesChart) {
        const base = casesChart.data.datasets[1].data;
        casesChart.data.datasets[1].data = base.map((v, i) =>
            v == null ? null : Math.round(v * scale * (1 + i * 0.01))
        );
        casesChart.update('none');
    }

    // Scale zoonotic chart
    const zooChart = charts['zoonoticChart'];
    if (zooChart) {
        zooChart.data.datasets[0].data =
            zooChart.data.datasets[0].data.map((v, i) =>
                Math.max(0, Math.round(v * (zoo / 12) * (1 - interv / 200)))
            );
        zooChart.update('none');
    }

    // Scale cumulative chart
    const cumChart = charts['cumulativeChart'];
    if (cumChart) {
        cumChart.data.datasets[1].data =
            cumChart.data.datasets[1].data.map(v =>
                v == null ? null : Math.round(v * scale)
            );
        cumChart.update('none');
    }

    // Rt chart: scale around intervention
    const rtChart = charts['rtChart'];
    if (rtChart) {
        rtChart.data.datasets[0].data =
            rtChart.data.datasets[0].data.map(v => +(v * scale).toFixed(2));
        rtChart.update('none');
    }
}

// ---------- Initialise ----------
function init() {
    renderCards('caseTelemetryCards', CASE_TELEMETRY);
    renderCards('bioCoeffCards', BIO_COEFFICIENTS);
    renderSliders();
    buildCharts();
    $('#resetBtn').addEventListener('click', resetSliders);
}

document.addEventListener('DOMContentLoaded', init);
