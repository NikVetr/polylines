/* Color parsing and blending. Colors are {r, g, b, a} with channels in 0-255 and alpha in 0-1. */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};
  const fail = (message) => { throw new SK.layout.SankeyError(message); };
  const clamp = (v, lo, hi) => Math.min(Math.max(v, lo), hi);

  const PALETTES = {
    polylines: ['#4E88B7', '#C9869F', '#7EA176', '#8D74AA', '#D45A2A', '#B7A45C', '#4C9FA6', '#F08C4A'],
    'okabe-ito': ['#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#999999'],
    tableau: ['#4E79A7', '#F28E2B', '#E15759', '#76B7B2', '#59A14F', '#EDC948', '#B07AA1', '#FF9DA7', '#9C755F', '#BAB0AC'],
    set2: ['#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3'],
    dark2: ['#1B9E77', '#D95F02', '#7570B3', '#E7298A', '#66A61E', '#E6AB02', '#A6761D', '#666666'],
    viridis: ['#440154', '#46327E', '#365C8D', '#277F8E', '#1FA187', '#4AC16D', '#9FDA3A', '#FDE725']
  };

  let canvasContext = null;
  function cssColor(text) {
    if (typeof document === 'undefined') return null;
    canvasContext = canvasContext || document.createElement('canvas').getContext('2d');
    canvasContext.fillStyle = '#010203';
    canvasContext.fillStyle = text;
    const out = canvasContext.fillStyle;
    return out === '#010203' ? null : parse(out);
  }

  function isMissing(value) {
    return value === null || value === undefined ||
      (typeof value === 'string' && /^(|na|nan|null)$/i.test(value.trim()));
  }

  // Accepts R/CSS hex (#RGB, #RGBA, #RRGGBB, #RRGGBBAA), rgb()/rgba(), R grayN/greyN and CSS names.
  // Missing values (NA, blank) return null; anything else unrecognised throws.
  function parse(value) {
    if (isMissing(value)) return null;
    if (typeof value === 'object') return value;
    const text = String(value).trim();
    let m = /^#([0-9a-f]{3,4}|[0-9a-f]{6}|[0-9a-f]{8})$/i.exec(text);
    if (m) {
      let hex = m[1];
      if (hex.length <= 4) hex = hex.split('').map((c) => c + c).join('');
      const v = (i) => parseInt(hex.slice(i, i + 2), 16);
      return { r: v(0), g: v(2), b: v(4), a: hex.length === 8 ? v(6) / 255 : 1 };
    }
    m = /^rgba?\(([^)]+)\)$/i.exec(text);
    if (m) {
      const parts = m[1].split(/[\s,/]+/).filter(Boolean);
      const channel = (s) => (s.endsWith('%') ? parseFloat(s) * 2.55 : parseFloat(s));
      const alpha = parts[3] === undefined ? 1 : (parts[3].endsWith('%') ? parseFloat(parts[3]) / 100 : parseFloat(parts[3]));
      const out = { r: channel(parts[0]), g: channel(parts[1]), b: channel(parts[2]), a: alpha };
      if (parts.length >= 3 && [out.r, out.g, out.b, out.a].every(Number.isFinite)) return out;
    }
    m = /^gr[ae]y(\d{1,3})$/i.exec(text);
    if (m && Number(m[1]) <= 100) {
      const level = Math.round(Number(m[1]) * 2.55);
      return { r: level, g: level, b: level, a: 1 };
    }
    if (/^transparent$/i.test(text)) return { r: 255, g: 255, b: 255, a: 0 };
    const css = cssColor(text);
    if (css) return css;
    return fail(`"${text}" is not a recognised color.`);
  }

  function isColor(value) {
    try {
      parse(value);
      return true;
    } catch (err) {
      return false;
    }
  }

  const hex2 = (v) => Math.round(clamp(v, 0, 255)).toString(16).padStart(2, '0');

  function toHex(c, keepAlpha) {
    if (!c) return null;
    const base = `#${hex2(c.r)}${hex2(c.g)}${hex2(c.b)}`;
    return (keepAlpha !== false && c.a < 1 ? base + hex2(c.a * 255) : base).toUpperCase();
  }

  function mix(c0, c1, t) {
    const u = clamp(t, 0, 1);
    return {
      r: c0.r + (c1.r - c0.r) * u,
      g: c0.g + (c1.g - c0.g) * u,
      b: c0.b + (c1.b - c0.b) * u,
      a: c0.a + (c1.a - c0.a) * u
    };
  }

  // Weighted channel average, including alpha, as in R's weighted_average_color().
  function weighted(cols, weights) {
    const total = weights.reduce((s, w) => s + w, 0);
    if (!(total > 0)) fail('Cannot average colors with zero total weight.');
    const out = { r: 0, g: 0, b: 0, a: 0 };
    cols.forEach((c, i) => {
      ['r', 'g', 'b', 'a'].forEach((k) => { out[k] += c[k] * weights[i] / total; });
    });
    return out;
  }

  // Composite onto an opaque background, as R's flatten_alpha_color() does for trunks.
  function flatten(c, bg) {
    const a = c.a;
    return { r: c.r * a + bg.r * (1 - a), g: c.g * a + bg.g * (1 - a), b: c.b * a + bg.b * (1 - a), a: 1 };
  }

  const withAlpha = (c, a) => ({ r: c.r, g: c.g, b: c.b, a });
  const opaque = (c) => withAlpha(c, 1);

  // SVG paint attributes: hex color plus separate opacity (broadest renderer support).
  function paint(c) {
    return { color: toHex(c, false), opacity: Math.round(clamp(c.a, 0, 1) * 1000) / 1000 };
  }

  function luminance(c) {
    const f = (v) => {
      const s = v / 255;
      return s <= 0.03928 ? s / 12.92 : Math.pow((s + 0.055) / 1.055, 2.4);
    };
    return 0.2126 * f(c.r) + 0.7152 * f(c.g) + 0.0722 * f(c.b);
  }

  SK.color = { PALETTES, parse, isColor, isMissing, toHex, mix, weighted, flatten, withAlpha, opaque, paint, luminance };
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
