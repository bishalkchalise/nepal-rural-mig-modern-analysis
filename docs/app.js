async function loadData() {
  const res = await fetch('data/overview.json');
  if (!res.ok) throw new Error('Could not load overview.json');
  return res.json();
}

function pct(v) { return `${(v*100).toFixed(1)}%`; }
function pp(v) { return `${v >= 0 ? '+' : ''}${v.toFixed(1)} pp`; }

function renderKPIs(data) {
  const el = document.getElementById('kpi-cards');
  data.headline_metrics.forEach(m => {
    const card = document.createElement('article');
    card.className = 'card';
    card.innerHTML = `
      <div class="muted">${m.label}</div>
      <div class="kpi">${pct(m.last)}</div>
      <div class="muted">${m.last_year} (${pp(m.change_pp)} since ${m.base_year})</div>
    `;
    el.appendChild(card);
  });
}

function drawTrend(series, metric) {
  const svg = document.getElementById('trendChart');
  while (svg.firstChild) svg.removeChild(svg.firstChild);

  const W = 900, H = 260, p = 40;
  const vals = series.map(r => ({ year: +r.year, value: +r[metric] })).filter(d => !Number.isNaN(d.value));
  const minX = Math.min(...vals.map(d => d.year));
  const maxX = Math.max(...vals.map(d => d.year));
  const minY = Math.min(...vals.map(d => d.value));
  const maxY = Math.max(...vals.map(d => d.value));

  const x = y => p + ((y - minX) / (maxX - minX || 1)) * (W - 2*p);
  const y = v => H - p - ((v - minY) / (maxY - minY || 1)) * (H - 2*p);

  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  path.setAttribute('fill', 'none');
  path.setAttribute('stroke', '#0b57d0');
  path.setAttribute('stroke-width', '3');
  path.setAttribute('d', vals.map((d,i) => `${i?'L':'M'} ${x(d.year)} ${y(d.value)}`).join(' '));
  svg.appendChild(path);

  vals.forEach(d => {
    const c = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
    c.setAttribute('cx', x(d.year)); c.setAttribute('cy', y(d.value)); c.setAttribute('r', 4); c.setAttribute('fill', '#0b57d0');
    svg.appendChild(c);

    const t = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    t.setAttribute('x', x(d.year)-12); t.setAttribute('y', H-12); t.setAttribute('font-size', '12'); t.textContent = d.year;
    svg.appendChild(t);
  });
}

function renderMetricControl(data) {
  const sel = document.getElementById('metricSelect');
  data.headline_metrics.forEach(m => {
    const o = document.createElement('option');
    o.value = m.metric; o.textContent = m.label;
    sel.appendChild(o);
  });
  const update = () => {
    drawTrend(data.national_series, sel.value);
    const m = data.headline_metrics.find(d => d.metric === sel.value);
    document.getElementById('trendMeta').textContent = `${m.label}: ${pct(m.base)} (${m.base_year}) → ${pct(m.last)} (${m.last_year}), ${pp(m.change_pp)}.`;
  };
  sel.addEventListener('change', update);
  update();
}

function renderRVS(data) {
  const tbody = document.querySelector('#rvsTable tbody');
  data.rvs_files.forEach(r => {
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>${r.file}</td><td>${r.rows.toLocaleString()}</td><td>${r.year_min ?? '-'}–${r.year_max ?? '-'}</td>`;
    tbody.appendChild(tr);
  });
}

function renderNEC(data) {
  const n = data.nec_summary;
  const items = [
    `Municipality-level rows: ${n.municipalities.toLocaleString()}`,
    `District-level rows: ${n.districts.toLocaleString()}`,
    `Total firms (municipality aggregate): ${n.firms_total.toLocaleString()}`,
    `Total employment (municipality aggregate): ${n.employment_total.toLocaleString()}`,
    `Mean formality index: ${n.formality_index_mean.toFixed(3)}`,
    `Entry cohort panel rows: ${n.entry_panel_rows.toLocaleString()}`
  ];
  const ul = document.getElementById('necList');
  items.forEach(x => {
    const li = document.createElement('li'); li.textContent = x; ul.appendChild(li);
  });
}

function renderProvince(data) {
  const ol = document.getElementById('provinceList');
  data.province_improvement_female_literacy.forEach(p => {
    const li = document.createElement('li');
    li.textContent = `${p.province}: ${pp(p.improvement_pp)} female literacy improvement`;
    ol.appendChild(li);
  });
}

loadData().then(data => {
  renderKPIs(data);
  renderMetricControl(data);
  renderRVS(data);
  renderNEC(data);
  renderProvince(data);
}).catch(err => {
  document.body.innerHTML = `<main class='wrap'><h1>Data load error</h1><p>${err.message}</p></main>`;
});
