let cy = null;

function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

/* ---------- path helpers: strip the common directory prefix ---------- */

function commonDir(paths) {
  const real = paths.filter(p => p && p.indexOf('/') >= 0);
  if (real.length === 0) return '';
  let prefix = real[0].substring(0, real[0].lastIndexOf('/') + 1);
  for (const p of real) {
    while (prefix && p.indexOf(prefix) !== 0) {
      prefix = prefix.substring(0, prefix.lastIndexOf('/', prefix.length - 2) + 1);
    }
  }
  return prefix;
}

/* ---------- file colors ---------- */

const FILE_PALETTE = [
  '#4e9af1', '#2ecc71', '#e67e22', '#9b59b6', '#1abc9c',
  '#f1c40f', '#e74c3c', '#95a5a6', '#d35400', '#7f8c8d'
];

/* Annotate elements in place: displayLabel + fileColor on parent boxes.
   Returns helpers for the legend and popups. */
function annotate(elements) {
  const files = [];
  elements.forEach(e => {
    const d = e.data;
    if (d && d.file && files.indexOf(d.file) < 0) files.push(d.file);
  });
  const prefix = commonDir(files);
  const rel = p => (p && p.indexOf(prefix) === 0) ? p.substring(prefix.length) : p;

  const fileColors = {};
  files.forEach((f, i) => { fileColors[f] = FILE_PALETTE[i % FILE_PALETTE.length]; });

  elements.forEach(e => {
    const d = e.data;
    if (!d) return;
    if (d.isParent) {
      d.fileColor = fileColors[d.file] || '#7f8c8d';
      const sub = rel(d.file) + (d.line ? ':' + d.line : '');
      const base = d.file ? d.file.split('/').pop() : '';
      const locLabel = base + (d.line ? ':' + d.line : '');
      // Unresolved boxes are already labeled "file:line" — don't print it twice.
      d.displayLabel = (d.label === locLabel) ? sub : d.label + '\n' + sub;
    }
  });
  return { rel: rel, fileColors: fileColors };
}

/* ---------- layout: follow the taint chain, one row per function box ---------- */

function computePositions(elements) {
  const nodes = elements.filter(e => e.data && !e.data.isParent && !e.data.source);
  const edges = elements.filter(e => e.data && e.data.source);
  const byId = {};
  nodes.forEach(n => { byId[n.data.id] = n; });

  const out = {}, indeg = {};
  nodes.forEach(n => { indeg[n.data.id] = 0; });
  edges.forEach(e => {
    if (byId[e.data.source] && byId[e.data.target]) {
      (out[e.data.source] = out[e.data.source] || []).push(e.data.target);
      indeg[e.data.target] += 1;
    }
  });

  // Order nodes by walking the chain from its start(s) (source first).
  const order = [], seen = {};
  const queue = nodes.filter(n => indeg[n.data.id] === 0).map(n => n.data.id);
  while (queue.length) {
    const id = queue.shift();
    if (seen[id]) continue;
    seen[id] = 1;
    order.push(id);
    const succ = (out[id] || []).filter(t => !seen[t]);
    queue.unshift(...succ);   // depth-first: keep following this chain
  }
  nodes.forEach(n => { if (!seen[n.data.id]) order.push(n.data.id); });

  // One row per function box, in order of first appearance along the chain.
  const COL_W = 250, ROW_H = 190;
  const rowOf = {}, pos = {};
  let nextRow = 0;
  order.forEach((id, i) => {
    const parent = byId[id].data.parent || id;
    if (!(parent in rowOf)) rowOf[parent] = nextRow++;
    pos[id] = { x: i * COL_W, y: rowOf[parent] * ROW_H };
  });
  return pos;
}

function computePositionsSafe(elements) {
  try {
    return computePositions(elements);
  } catch (err) {
    console.error('preset layout failed, falling back to cose', err);
    return null;
  }
}

/* ---------- legend ---------- */

function renderLegend(fileColors, rel) {
  const legend = document.getElementById('file-legend');
  if (!legend) return;
  const entries = Object.keys(fileColors).filter(f => f && f.indexOf('/') >= 0);
  legend.innerHTML = '<div class="legend-title">Files</div>' + entries.map(f =>
    '<div class="legend-entry"><span class="legend-swatch" style="background:' +
    fileColors[f] + '"></span>' + escapeHtml(rel(f)) + '</div>'
  ).join('');
  legend.style.display = entries.length ? 'block' : 'none';
}

/* ---------- render ---------- */

let currentRel = p => p;

function render(elements) {
  if (cy) cy.destroy();

  const ann = annotate(elements);
  currentRel = ann.rel;
  renderLegend(ann.fileColors, ann.rel);
  const positions = computePositionsSafe(elements);

  cy = cytoscape({
    container: document.getElementById('cy'),
    elements: elements,
    style: [
      // Function boxes, tinted by file
      { selector: 'node[?isParent]', style: {
          'background-color': 'data(fileColor)',
          'background-opacity': 0.12,
          'label': 'data(displayLabel)',
          'color': '#ecf0f1',
          'text-wrap': 'wrap',
          'text-valign': 'top',
          'text-halign': 'center',
          'text-margin-y': -8,
          'font-size': '18px',
          'font-weight': 'bold',
          'shape': 'roundrectangle',
          'padding': '30px',
          'border-width': 3,
          'border-color': 'data(fileColor)'
      }},
      // Unresolved function boxes (couldn't match a known function)
      { selector: 'node[?isParent][!resolved]', style: {
          'border-style': 'dashed'
      }},
      // Taint steps
      { selector: 'node[!isParent]', style: {
          'background-color': 'data(color)',
          'label': 'data(label)',
          'color': '#fff',
          'text-valign': 'center',
          'text-halign': 'center',
          'font-size': '16px',
          'text-wrap': 'ellipsis',
          'text-max-width': '200px',
          'width': 'label',
          'height': 'label',
          'padding': '14px',
          'shape': 'roundrectangle'
      }},
      // Flow inside one function
      { selector: 'edge[lineStyle="solid"]', style: {
          'width': 2.5,
          'line-color': 'data(color)',
          'target-arrow-color': 'data(color)',
          'target-arrow-shape': 'triangle',
          'curve-style': 'bezier',
          'arrow-scale': 1.4
      }},
      // Flow crossing into another function
      { selector: 'edge[lineStyle="dashed"]', style: {
          'width': 3,
          'line-color': 'data(color)',
          'target-arrow-color': 'data(color)',
          'target-arrow-shape': 'triangle',
          'curve-style': 'bezier',
          'line-style': 'dashed',
          'arrow-scale': 1.6,
          'label': 'data(label)',
          'font-size': '13px',
          'color': '#e74c3c',
          'text-background-color': '#1a1a2e',
          'text-background-opacity': 0.8,
          'text-background-padding': '3px'
      }},
      { selector: 'node:selected', style: {
          'border-width': 3,
          'border-color': '#fff'
      }}
    ],
    layout: positions
      ? { name: 'preset', positions: n => positions[n.id()], fit: true, padding: 60 }
      : { name: 'cose', animate: false, fit: true, padding: 60,
          nodeDimensionsIncludeLabels: true }
  });

  cy.fit(60);

  cy.on('tap', 'node', function(e) {
    const d = e.target.data();

    let color, typeLabel, codeHtml;

    if (d.isParent) {
      // Function box - show function definition
      color = d.fileColor || '#34495e';
      typeLabel = 'function';

      if (d.code_context && d.code_context.length > 0) {
        const lines = d.code_context.map(ctx => {
          const lineNum = String(ctx.line).padStart(4, ' ');
          let text = escapeHtml(ctx.text);
          const isTarget = ctx.line === d.line;
          const lineClass = isTarget ? 'target-line' : '';
          return '<div class="code-line ' + lineClass + '"><span class="line-num">' + lineNum + '</span> ' + text + '</div>';
        }).join('');
        codeHtml = '<pre>' + lines + '</pre>';
      } else {
        codeHtml = '<pre>' + escapeHtml(d.label) + '</pre>';
      }
    } else {
      // Taint step node
      color = d.type === 'source' ? '#2ecc71' :
              d.type === 'sink' ? '#e74c3c' :
              d.type === 'call' ? '#9b59b6' : '#3498db';
      typeLabel = d.type;

      if (d.code_context && d.code_context.length > 0) {
        const lines = d.code_context.map(ctx => {
          const lineNum = String(ctx.line).padStart(4, ' ');
          let text = escapeHtml(ctx.text);

          // Highlight the tainted portion on the target line
          if (ctx.line === d.line && d.col > 0 && d.taint_len > 0) {
            const col = d.col - 1; // 0-indexed
            const before = text.substring(0, col);
            const tainted = text.substring(col, col + d.taint_len);
            const after = text.substring(col + d.taint_len);
            text = before + '<span style="background-color:' + color + ';color:#fff;padding:1px 3px;border-radius:2px;">' + tainted + '</span>' + after;
          }

          const isTarget = ctx.line === d.line;
          const lineClass = isTarget ? 'target-line' : '';
          return '<div class="code-line ' + lineClass + '"><span class="line-num">' + lineNum + '</span> ' + text + '</div>';
        }).join('');
        codeHtml = '<pre>' + lines + '</pre>';
      } else {
        codeHtml = '<pre>' + escapeHtml(d.label) + '</pre>';
      }
    }

    // Get node position on screen
    const pos = e.target.renderedPosition();
    const container = document.getElementById('cy').getBoundingClientRect();

    // Show popup near the node
    const popup = document.getElementById('code-popup');
    popup.innerHTML =
      '<div class="popup-header">' +
      '<span class="type-badge type-' + typeLabel + '">' + typeLabel + '</span>' +
      '<span class="popup-close" onclick="document.getElementById(\'code-popup\').classList.add(\'hidden\')">&times;</span>' +
      '</div>' +
      '<div class="popup-file">' + escapeHtml(currentRel(d.file)) + ':' + d.line + '</div>' +
      '<div class="popup-body">' + codeHtml + '</div>';
    popup.style.left = (container.left + pos.x + 50) + 'px';
    popup.style.top = (pos.y - 50) + 'px';
    popup.style.borderColor = color;
    popup.classList.remove('hidden');

  });

  // Hide popup when clicking on background
  cy.on('tap', function(e) {
    if (e.target === cy) {
      document.getElementById('code-popup').classList.add('hidden');
    }
  });
}

// Drag the code popup by its header
(function () {
  let drag = null;
  document.addEventListener('mousedown', e => {
    const header = e.target.closest('.popup-header');
    if (!header || e.target.classList.contains('popup-close')) return;
    const popup = document.getElementById('code-popup');
    drag = { dx: e.clientX - popup.offsetLeft, dy: e.clientY - popup.offsetTop };
    e.preventDefault();
  });
  document.addEventListener('mousemove', e => {
    if (!drag) return;
    const popup = document.getElementById('code-popup');
    popup.style.left = (e.clientX - drag.dx) + 'px';
    popup.style.top = (e.clientY - drag.dy) + 'px';
  });
  document.addEventListener('mouseup', () => { drag = null; });
})();

// Populate findings list
const list = document.getElementById('findings-list');
FINDINGS.forEach((f, i) => {
  const div = document.createElement('div');
  div.className = 'finding-item';
  div.innerHTML = '<div class="rule-id">' + escapeHtml(f.rule_id) + '</div><div class="file">' + escapeHtml(f.file.split('/').pop()) + '</div>';
  div.onclick = () => {
    document.querySelectorAll('.finding-item').forEach(el => el.classList.remove('active'));
    div.classList.add('active');
    document.getElementById('code-popup').classList.add('hidden');
    render(f.elements);
  };
  list.appendChild(div);
});

// Render first finding by default
if (FINDINGS.length > 0) {
  document.querySelector('.finding-item').classList.add('active');
  render(FINDINGS[0].elements);
}
