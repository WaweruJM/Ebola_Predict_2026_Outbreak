/* ============================================================
   Ebola Outbreak Analysis Portal — Front-End Logic
   ============================================================ */

const DATA_URL = 'data/ebola_data.json';
let DATA = {
    case_telemetry: [], bio_coefficients: [], chart_series: {},
    zones: [], sliders: [], primary_parameters: {}, performance: {}
};
let charts = {};
let originalChartData = {}; // Stores baseline data for resetting graphs

const $ = (sel) => document.querySelector(sel);

// ---------- UI Components ----------
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
        
        // Bind event only once
        if (!input.dataset.bound) {
            input.dataset.bound = "1";
            input.addEventListener('input', () => {
                valEl.textContent = input.value;
                updateCharts();
            });
        }
    });
}

function resetSliders() {
    // 1. Reset the slider UI elements
    DATA.sliders.forEach(s => {
        const input = document.getElementById(`slider-${s.id}`);
        if (input) {
            input.value = s.value;
            document.getElementById(`val-${s.id}`).textContent = s.value;
        }
    });

    // 2. Reset the Predictive Chart to its original state
    const pc = charts.predictiveChart;
    if (pc && originalChartData.predictive) {
        pc.data = JSON.parse(JSON.stringify(originalChartData.predictive));
        pc.update('none');
    }

    // 3. Reset the Coefficients Chart
    const cc = charts.coeffChart;
    if (cc && originalChartData.coeff) {
        cc.data = JSON.parse(JSON.stringify(originalChartData.coeff));
        cc.update('none');
    }
}

// ---------- Chart Plugins ----------
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
            ctx.fillRect(x0, chartArea.top, x1 - x0, chartArea.bottom - chartArea.top);
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
            ctx.moveTo(chartArea.left, py);
            ctx.lineTo(chartArea.right, py);
            ctx.stroke();
            ctx.setLineDash([]);
            ctx.fillStyle = color;
            ctx.font = '600 10px Inter, sans-serif';
            ctx.textAlign = 'right';
            ctx.fillText(`${label} = ${value.toFixed(3)}`, chartArea.right - 6, py - 3);
        });
        ctx.restore();
    }
};
Chart.register(paramLinesPlugin);

// ---------- Helpers ----------
function padTail(n = 0) { return new Array(Math.max(0, n || 0)).fill(null); }
function padHead(len, arr = []) { return new Array(len).fill(null).concat(arr); }

function chartOptions(type, yTitle = 'Count') {
    return {
        responsive: true, maintainAspectRatio: false,
        interaction: { mode: 'nearest', intersect: true }, // Prevents jumping on click
        onClick: () => {}, // Disables click deflection
        layout: { padding: { top: 8, right: 8, bottom: 8, left: 8 } },
        plugins: {
            legend: { 
                position: 'right', align: 'start',
                labels: { boxWidth: 14, boxHeight: 10, padding: 12, font: { size: 11, family: 'Inter' }, color: '#1f2d3d' } 
            },
            tooltip: { 
                backgroundColor: 'rgba(14,42,74,0.95)', 
                titleFont: { size: 12, family: 'Inter' }, 
                bodyFont: { size: 11, family: 'Inter' }, 
                padding: 10, cornerRadius: 6 
            }
        },
        scales: {
            x: {
                title: { display: true, text: 'Time (Date)', font: { size: 11, weight: '600', family: 'Inter' }, color: '#1f2d3d' },
                ticks: { font: { size: 10, family: 'Inter' }, color: '#6b7c93', maxRotation: 0, autoSkip: true, maxTicksLimit: 12 },
                grid: { color: 'rgba(0,0,0,0.04)' }
            },
            y: {
                beginAtZero: true,
                title: { display: true, text: yTitle, font: { size: 11, weight: '600', family: 'Inter' }, color: '#1f2d3d' },
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

// ---------- Chart Builder ----------
function buildCharts() {
    const col = $('#chartsColumn');
    col.innerHTML = '';
    const S = DATA.chart_series;
    const Pf = DATA.performance || {};
    const P = DATA.primary_parameters || {};

    // Merge observed dates with projection dates for the predictive chart
    const allDates = S.months.concat(S.projection_dates || []);

    const defs = [
        {
            id: 'confirmedChart',
            title: 'Confirmed Cases, Deaths & Recoveries',
            subtitle: 'Observed series (confirmed only)',
            type: 'line', yTitle: 'Cumulative count', zones: null,
            build: () => ({
                labels: S.months,
                datasets: [
                    { label: 'Confirmed', data: S.confirmed_observed, borderColor: '#e74c3c', backgroundColor: 'rgba(231,76,60,0.14)', borderWidth: 2.6, tension: 0.35, fill: true, pointRadius: 2 },
                    { label: 'Recovered', data: S.recovered_observed, borderColor: '#16a085', borderWidth: 2.0, tension: 0.35, pointRadius: 2 },
                    { label: 'Deaths', data: S.deaths_observed, borderColor: '#7f8c8d', borderWidth: 2.0, tension: 0.35, pointRadius: 1 }
                ]
            })
        },
        {
            id: 'sirPopChart',
            title: 'SIR Compartments',
            subtitle: 'N = S = 20,000,000',
            type: 'line', yTitle: 'Individuals', zones: null,
            build: () => ({
                labels: S.months,
                datasets: [
                    { label: 'Infectious (I)', data: S.sir_I, borderColor: '#e74c3c', backgroundColor: 'rgba(231,76,60,0.10)', borderWidth: 2.4, tension: 0.35, fill: true, pointRadius: 2 },
                    { label: 'Recovered (R)', data: S.sir_R, borderColor: '#16a085', borderWidth: 2, tension: 0.35, pointRadius: 1 },
                    { label: 'Deaths (D)', data: S.sir_D, borderColor: '#7f8c8d', borderWidth: 1.6, tension: 0.35, pointRadius: 0 }
                ]
            })
        },
        {
            id: 'coeffChart',
            title: 'Biological Coefficients (beta, gamma, mu)',
            subtitle: 'Per-day rates',
            type: 'line', yTitle: 'Rate (/day)', zones: null,
            build: () => ({
                labels: S.months,
                datasets: [
                    { label: 'beta (transmission)', data: S.coeff_beta, borderColor: '#c0392b', borderWidth: 2.4, pointRadius: 0, tension: 0.1, borderDash: [6,4] },
                    { label: 'gamma (recovery)', data: S.coeff_gamma, borderColor: '#27ae60', borderWidth: 2.4, pointRadius: 0, tension: 0.1, borderDash: [6,4] },
                    { label: 'mu (mortality)', data: S.coeff_mu, borderColor: '#8e44ad', borderWidth: 2.4, pointRadius: 0, tension: 0.1, borderDash: [6,4] }
                ]
            })
        },
        {
            id: 'predictiveChart',
            title: 'Predictive Model',
            subtitle: 'Observed vs Fitted vs 14-Day Projection',
            type: 'line', yTitle: 'Cumulative confirmed cases', zones: true, showPerf: true,
            build: () => ({
                labels: allDates,
                datasets: [
                    // The observed dataset is padded with nulls so it only draws up to the last observed date.
                    { label: 'Observed confirmed', data: S.confirmed_observed.concat(padTail(S.projection_dates?.length)), borderColor: '#1f2d3d', borderWidth: 2.6, tension: 0.3, pointRadius: 2, adjustable: false },
                    // The fitted dataset continues into the projection period.
                    { label: 'SIR fitted', data: S.seir_fitted_I.concat(padTail(S.projection_dates?.length)), borderColor: '#e74c3c', borderWidth: 2.2, borderDash: [6,4], tension: 0.3, pointRadius: 1, adjustable: true },
                    // The projection dataset starts where the observed data ends.
                    { label: 'Projection (14d)', data: padHead(S.months.length, S.projection_I || []), borderColor: '#8e44ad', borderWidth: 2.6, borderDash: [3,3], tension: 0.3, pointRadius: 1, adjustable: true }
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
        if (def.zones) {
            opts.plugins.zoneBands = { zones: DATA.zones };
        }
        if (def.id === 'sirPopChart') {
            opts.plugins.paramLines = {
                lines: [
                    { value: P.beta || 0.42,  label: 'beta',  color: '#c0392b' },
                    { value: P.gamma || 0.13, label: 'gamma', color: '#27ae60' },
                    { value: P.mu || 0.05,    label: 'mu',    color: '#8e44ad' }
                ]
            };
        }

        charts[def.id] = new Chart(ctx, {
            type: def.type,
            data: def.build(),
            options: opts
        });

        // Store baseline data for resetting
        if (def.id === 'predictiveChart') {
            originalChartData.predictive = JSON.parse(JSON.stringify(charts[def.id].data));
        }
        if (def.id === 'coeffChart') {
            originalChartData.coeff = JSON.parse(JSON.stringify(charts[def.id].data));
        }
    });
}

// ---------- Slider Updates ----------
function updateCharts() {
    const beta = parseFloat($('#slider-beta')?.value ?? 0.42);
    const gamma = parseFloat($('#slider-gamma')?.value ?? 0.13);
    const mu = parseFloat($('#slider-mu')?.value ?? 0.05);
    const interv = parseFloat($('#slider-interv')?.value ?? 35);

    // Adjust the reproduction number based on sliders
    const rt_eff = (beta / (gamma + mu)) * (1 - interv / 100);
    // A scale factor to adjust the fitted curve to show the effect of user parameters
    const scale = Math.max(0.3, Math.min(2.0, rt_eff));

    // 1. Update the Predictive Chart (only the adjustable datasets)
    const pc = charts.predictiveChart;
    if (pc && originalChartData.predictive) {
        // We scale the baseline data to avoid compounding effects
        const baseFitted = originalChartData.predictive.datasets[1].data;
        const baseProj = originalChartData.predictive.datasets[2].data;

        pc.data.datasets[1].data = baseFitted.map(v => v == null ? null : +(v * scale).toFixed(2));
        pc.data.datasets[2].data = baseProj.map(v => v == null ? null : +(v * scale).toFixed(2));
        
        pc.update('none'); // 'none' prevents animation jitter
    }

    // 2. Update the Coefficients Chart (adjusting the flat lines)
    const cc = charts.coeffChart;
    if (cc) {
        cc.data.datasets[0].data = cc.data.datasets[0].data.map(() => +beta.toFixed(3));
        cc.data.datasets[1].data = cc.data.datasets[1].data.map(() => +gamma.toFixed(3));
        cc.data.datasets[2].data = cc.data.datasets[2].data.map(() => +mu.toFixed(3));
        cc.update('none');
    }
}

// ---------- Data Loading ----------
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
        { label: 'Policy factor', value: 0, unit: '', delta: '' }
    ],
    chart_series: {
        months: [], confirmed_observed: [], recovered_observed: [], deaths_observed: [],
        seir_fitted_I: [], projection_dates: [], projection_I: [],
        sir_I: [], sir_R: [], sir_D: [],
        coeff_beta: [], coeff_gamma: [], coeff_mu: []
    },
    zones: [],
    sliders: [
        { id: 'beta', label: 'Transmission Rate (beta)', min: 0.05, max: 1.5, step: 0.01, value: 0.5 },
        { id: 'gamma', label: 'Recovery Rate (gamma)', min: 0.02, max: 0.5, step: 0.01, value: 0.15 },
        { id: 'mu', label: 'Mortality Rate (mu)', min: 0.0, max: 0.5, step: 0.01, value: 0.05 },
        { id: 'zoonotic', label: 'Zoonotic Spillover', min: 0, max: 50, step: 1, value: 12 },
        { id: 'interv', label: 'Intervention Efficacy', min: 0, max: 100, step: 1, value: 35 }
    ],
    primary_parameters: { beta: 0.5, gamma: 0.15, mu: 0.05 },
    performance: {}
};

async function loadData() {
    try {
        // Cache-busting to ensure the latest data is always fetched
        const res = await fetch(`${DATA_URL}?v=${Date.now()}`, { cache: 'no-store' });
        if (!res.ok) throw new Error(`HTTP ${res.status}`);
        DATA = await res.json();
    } catch (err) {
        console.warn('Could not load fresh data. Using fallback.', err);
        DATA = FALLBACK_DATA;
    }
}

// ---------- Initialization ----------
async function init() {
    await loadData();
    
    // Populate cards
    renderCards('caseTelemetryCards', DATA.case_telemetry);
    renderCards('bioCoeffCards', DATA.bio_coefficients);
    
    // Setup sliders
    renderSliders();
    
    // Render all charts
    buildCharts();
    
    // Wire up the reset button
    $('#resetBtn').addEventListener('click', resetSliders);
    
    // Add timestamp to footer if available
    if (DATA.generated_at) {
        const stamp = document.createElement('span');
        stamp.style.fontSize = '0.75rem';
        stamp.style.marginLeft = '12px';
        stamp.style.opacity = '0.7';
        stamp.textContent = `Last refreshed: ${new Date(DATA.generated_at).toLocaleString()}`;
        const footer = document.querySelector('.portal-footer');
        if (footer) footer.appendChild(stamp);
    }
}

// Start
document.addEventListener('DOMContentLoaded', init);
