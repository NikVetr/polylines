/* Example datasets, stored as the same CSV files the R functions read (see example_3.R).
 * Each example lists its files by role; loading one goes through the normal upload path.
 */
(function (root) {
  'use strict';
  const SK = root.SK = root.SK || {};

  // Deterministic long-format survey: one row per respondent and wave; some answers drift.
  function surveyTable() {
    const answers = ['Agree', 'Neutral', 'Disagree'];
    let seed = 7;
    const next = () => {
      seed = (seed * 16807) % 2147483647;
      return seed / 2147483647;
    };
    const rows = ['respondent,wave,answer'];
    for (let id = 1; id <= 40; id++) {
      let a = Math.floor(next() * 3);
      for (let wave = 1; wave <= 3; wave++) {
        if (wave > 1 && next() < 0.35) a = Math.max(0, Math.min(2, a + (next() < 0.6 ? -1 : 1)));
        const skipped = wave === 3 && next() < 0.1;
        rows.push(`R${String(id).padStart(2, '0')},${wave},${skipped ? 'NA' : answers[a]}`);
      }
    }
    return `${rows.join('\n')}\n`;
  }

  // Deterministic cohort of fictitious patients, one row each, followed from diagnosis to two
  // years. Rows are sorted by trajectory, so node order (first appearance) runs best to worst.
  function cohortTable() {
    let seed = 11;
    const next = () => {
      seed = (seed * 16807) % 2147483647;
      return seed / 2147483647;
    };
    const pick = (probs) => {
      let u = next();
      const i = probs.findIndex((p) => (u -= p) < 0);
      return i < 0 ? probs.length - 1 : i;
    };
    const stages = ['Stage I', 'Stage II', 'Stage III', 'Stage IV'];
    const treatments = ['Surgery', 'Radiation', 'Chemotherapy', 'Immunotherapy', 'Palliative care'];
    const responses = ['Complete response', 'Partial response', 'Stable disease', 'Progression'];
    const statuses = ['Disease-free', 'Living with disease', 'Deceased'];
    const treatmentByStage = [[0.7, 0.3, 0, 0, 0], [0.45, 0.2, 0.35, 0, 0], [0.2, 0.25, 0.55, 0, 0], [0, 0, 0.4, 0.35, 0.25]];
    const responseByTreatment = [[0.65, 0.15, 0.12, 0.08], [0.45, 0.25, 0.18, 0.12], [0.25, 0.3, 0.25, 0.2],
      [0.2, 0.3, 0.25, 0.25], [0, 0.1, 0.45, 0.45]];
    const statusByResponse = [[0.85, 0.1, 0.05], [0.4, 0.45, 0.15], [0.15, 0.55, 0.3], [0.03, 0.32, 0.65]];
    const patients = Array.from({ length: 200 }, (_, k) => {
      const stage = pick([0.3, 0.3, 0.25, 0.15]);
      const treatment = pick(treatmentByStage[stage]);
      let response = pick(responseByTreatment[treatment]);
      // Later stages respond a step worse more often.
      if (response < 3 && next() < 0.1 * stage) response++;
      return { id: `P${String(k + 1).padStart(3, '0')}`, path: [stage, treatment, response, pick(statusByResponse[response])] };
    });
    patients.sort((a, b) => a.path.reduce((d, v, s) => d || v - b.path[s], 0));
    const rows = patients.map((p) => [p.id, stages[p.path[0]], treatments[p.path[1]], responses[p.path[2]], statuses[p.path[3]]].join(','));
    return `patient_id,diagnosis,first treatment,response at 6 months,status at 2 years\n${rows.join('\n')}\n`;
  }

  SK.examples = [
    {
      id: 'labeled',
      title: 'Labeled 4–2–3 with node bodies',
      mode: 'layered',
      stages: ['labeled_stage_1.csv', 'labeled_stage_2.csv'],
      loc: 'labeled_loc.csv',
      colors: ['labeled_colors_1.csv', 'labeled_colors_2.csv'],
      mids: ['labeled_mid_1.csv', 'labeled_mid_2.csv'],
      params: { n: 120, node_label_pad: 0.012, node_label_pad_frac: 0.03 },
      style: { colorBy: 'file', gradient: true, gradientMode: 'path', useMid: false },
      files: {
        'labeled_stage_1.csv': '0.0832,0.032\n0.0384,0.1088\n0.096,0.032\n0.0384,0.1088\n',
        'labeled_stage_2.csv': '0.1088,0.0896,0.0576\n0.0448,0.1216,0.1152\n',
        'labeled_loc.csv': [
          'set,node,x,y,label,cex,font,color',
          '1,1,-0.06,0.88,Clinical A,0.7,2,#245C8A',
          '1,2,0.10,0.64,Long input B,0.7,2,#B34F2C',
          '1,3,-0.02,0.38,Molecular C,0.7,2,#4B8B63',
          '1,4,0.16,0.14,Trait D,0.7,2,#7A5DB0',
          '2,1,1.20,0.69,Mediator,0.74,2,#4B9AB8',
          '2,2,1.50,0.31,Second mediator,0.74,2,#9070A8',
          '3,1,2.90,0.82,Outcome 1,0.7,2,#1D7A72',
          '3,2,2.62,0.50,Longer outcome,0.7,2,#8B6F2A',
          '3,3,3.02,0.18,Outcome 3,0.7,2,#8F3D6A'
        ].join('\n') + '\n',
        'labeled_colors_1.csv': '#2B7FB8BB,#B7A45CBB\n#D45A2ABB,#C9869FBB\n#4E9F6EBB,#89BF7ABB\n#7A5DB0BB,#D19AD6BB\n',
        'labeled_colors_2.csv': '#4B9AB8BB,#9BA64ABB,#CB8FA2BB\n#6AAE9ABB,#B09D4CBB,#A66DB2BB\n',
        'labeled_mid_1.csv': '#F2B84B,NA\nNA,#E28AA3\n#56B6A9,NA\n#8B5FBF,#DB6B92\n',
        'labeled_mid_2.csv': '#50B5A8,NA,#C86A9A\nNA,#C9AA4B,#A16FC0\n'
      }
    },
    {
      id: 'pathways',
      title: 'Patient pathways (wide table import)',
      table: 'patient_pathways.csv',
      importSpec: { format: 'wide', columns: ['diagnosis', 'treatment', 'response'], weight: 'patients', missing: 'drop' },
      importOptions: { order: 'appearance' },
      labels: 'above',
      params: { node_label_pad: 0.02 },
      style: { colorBy: 'source', palette: 'tableau', opacity: 0.8, gradient: true, gradientMode: 'path' },
      files: {
        'patient_pathways.csv': [
          'diagnosis,treatment,response,patients',
          'Stage I,Surgery,Remission,120', 'Stage I,Surgery,Stable,18', 'Stage I,Radiation,Remission,35',
          'Stage I,Radiation,Stable,9', 'Stage II,Surgery,Remission,64', 'Stage II,Surgery,Stable,30',
          'Stage II,Chemotherapy,Remission,41', 'Stage II,Chemotherapy,Progression,22', 'Stage II,Radiation,Stable,17',
          'Stage III,Chemotherapy,Remission,28', 'Stage III,Chemotherapy,Stable,36', 'Stage III,Chemotherapy,Progression,44',
          'Stage III,Radiation,Progression,15', 'Stage IV,Chemotherapy,Stable,12', 'Stage IV,Chemotherapy,Progression,38',
          'Stage IV,Palliative,Progression,26', 'Stage IV,Palliative,Stable,5'
        ].join('\n') + '\n'
      }
    },
    {
      id: 'cohort',
      title: 'Patient cohort, one row per patient (wide table import)',
      table: 'patient_cohort.csv',
      importSpec: {
        format: 'wide', id: 'patient_id', weight: null, missing: 'drop',
        columns: ['diagnosis', 'first treatment', 'response at 6 months', 'status at 2 years']
      },
      importOptions: { order: 'appearance' },
      labels: 'above',
      params: { node_label_pad: 0.02 },
      style: { colorBy: 'source', palette: 'tableau', opacity: 0.8, gradient: true, gradientMode: 'path', substrands: true },
      files: { 'patient_cohort.csv': cohortTable() }
    },
    {
      id: 'survey',
      title: 'Survey waves (long table import)',
      table: 'survey_waves.csv',
      importSpec: { format: 'long', id: 'respondent', stage: 'wave', category: 'answer', weight: null, stageOrder: 'auto', missing: 'keep' },
      importOptions: { order: 'appearance' },
      labels: 'above',
      style: { colorBy: 'target', palette: 'okabe-ito', opacity: 0.75, gradient: true, gradientMode: 'merge' },
      files: { 'survey_waves.csv': surveyTable() }
    },
    {
      id: 'gradients',
      title: 'Multi-stage gradients (README figure)',
      mode: 'layered',
      stages: ['gradient_stage_1.csv', 'gradient_stage_2.csv', 'gradient_stage_3.csv'],
      loc: 'gradient_loc.csv',
      colors: ['gradient_colors_1.csv', 'gradient_colors_2.csv', 'gradient_colors_3.csv'],
      mids: ['gradient_mid_1.csv', 'gradient_mid_2.csv', 'gradient_mid_3.csv'],
      params: { n: 160, merge_frac: 0.075 },
      style: { colorBy: 'file', gradient: true, gradientMode: 'path', useMid: true, showPoints: true },
      files: {
        'gradient_stage_1.csv': '0.35,0.25,0.05,0.10\n0.10,0.35,0.20,0.05\n0.05,0.10,0.30,0.20\n',
        'gradient_stage_2.csv': '0.50,0,0,0\n0,0.70,0,0\n0,0,0.55,0\n0,0,0,0.35\n',
        'gradient_stage_3.csv': '0.25,0.15,0.10\n0.10,0.40,0.20\n0.05,0.15,0.35\n0.12,0.06,0.17\n',
        'gradient_loc.csv': [
          'set,node,x,y,color,merge_col',
          '1,1,0,3.0,#245C8A,#245C8AEB', '1,2,0,2.0,#5A7D3A,#5A7D3AEB', '1,3,0,1.0,#9B4D65,#9B4D65EB',
          '2,1,1.5,3.3,#4078A8,#4078A8EB', '2,2,1.5,2.4,#7C8F47,#7C8F47EB', '2,3,1.5,1.5,#B8758B,#B8758BEB',
          '2,4,1.5,0.6,#7A5B9A,#7A5B9AEB',
          '3,1,2.5,3.3,#4B9AB8,#4B9AB8EB', '3,2,2.5,2.4,#9BA64A,#9BA64AEB', '3,3,2.5,1.5,#CB8FA2,#CB8FA2EB',
          '3,4,2.5,0.6,#9070A8,#9070A8EB',
          '4,1,4.0,3.0,#1D7A72,#1D7A72EB', '4,2,4.0,2.0,#8B6F2A,#8B6F2AEB', '4,3,4.0,1.0,#8F3D6A,#8F3D6AEB'
        ].join('\n') + '\n',
        'gradient_colors_1.csv': '#4E88B7,#699E6D,#C9869F,#8D74AA\n#5F98C5,#8DA65C,#B97991,#846AA3\n#6BA8B8,#A59D4E,#CD8BA5,#A47BB3\n',
        'gradient_colors_2.csv': '#5FA3C4,NA,NA,NA\nNA,#A3AA55,NA,NA\nNA,NA,#CE91AA,NA\nNA,NA,NA,#9A78B2\n',
        'gradient_colors_3.csv': '#4C9FA6,#7EA176,#B480A1\n#6AAE9A,#B09D4C,#C6859C\n#7EB2B8,#B7A45C,#C9548F\n#7292B4,#A78E55,#A66DB2\n',
        'gradient_mid_1.csv': '#F2B84B,NA,#D75A7C,#8B5FBF\nNA,#C8A64D,#E28AA3,NA\n#56B6A9,NA,#DB6B92,#A16FC0\n',
        'gradient_mid_2.csv': 'NA,NA,NA,NA\nNA,NA,NA,NA\nNA,NA,NA,NA\nNA,NA,NA,NA\n',
        'gradient_mid_3.csv': '#50B5A8,NA,#C86A9A\nNA,#C9AA4B,#D27694\n#73B9C4,NA,#DE5D98\n#6A91BF,#B59B58,NA\n'
      }
    },
    {
      id: 'crowded',
      title: 'Crowded nodes with adjust_loc',
      mode: 'layered',
      stages: ['crowded_stage_1.csv', 'crowded_stage_2.csv'],
      loc: 'crowded_loc.csv',
      params: { n: 150, merge_frac: 0.06, adjust_loc: true, min_node_gap: 0.015, adjust_loc_expand: false },
      style: { colorBy: 'source', palette: 'tableau', opacity: 0.8, gradient: true, gradientMode: 'path', showPoints: true },
      files: {
        'crowded_stage_1.csv': [
          '0.08,0.02,0.00,0.01,0.00', '0.03,0.06,0.02,0.00,0.00', '0.00,0.04,0.05,0.02,0.00',
          '0.00,0.00,0.03,0.06,0.02', '0.02,0.00,0.00,0.03,0.06'
        ].join('\n') + '\n',
        'crowded_stage_2.csv': [
          '0.065,0.0325,0.010833,0.021667', '0.02,0.06,0.03,0.01', '0.009091,0.027273,0.045455,0.018182',
          '0,0.021818,0.065455,0.032727', '0.026667,0,0.017778,0.035556'
        ].join('\n') + '\n',
        'crowded_loc.csv': [
          'set,node,x,y',
          '1,1,0,0.12', '1,2,0,0.34', '1,3,0,0.365', '1,4,0,0.62', '1,5,0,0.88',
          '2,1,1.35,0.14', '2,2,1.35,0.33', '2,3,1.35,0.355', '2,4,1.35,0.58', '2,5,1.35,0.86',
          '3,1,2.7,0.16', '3,2,2.7,0.43', '3,3,2.7,0.455', '3,4,2.7,0.84'
        ].join('\n') + '\n'
      }
    },
    {
      id: 'fanin',
      title: 'Dense fan-in with boundary routing',
      mode: 'layered',
      stages: ['fanin_stage_1.csv'],
      loc: 'fanin_loc.csv',
      params: {
        n: 220, merge_frac: 0.045, route: 'boundary', boundary_group: 'target',
        boundary_compensation: 'full', adjust_loc: true, min_node_gap: 0.0015, adjust_loc_expand: true
      },
      style: { colorBy: 'single', singleColor: '#5FA3C4', opacity: 0.72, gradient: false, showPoints: true },
      files: {
        'fanin_stage_1.csv': '0.010\n0.060\n0.012\n0.085\n0.014\n0.070\n0.020\n0.105\n0.018\n0.075\n0.013\n0.055\n0.011\n0.045\n0.010\n',
        'fanin_loc.csv': ['set,node,x,y']
          .concat(Array.from({ length: 15 }, (_, i) => `1,${i + 1},0,${(0.86 - i * 0.1).toFixed(2)}`))
          .concat(['2,1,2.7,-0.5']).join('\n') + '\n'
      }
    },
    {
      id: 'graph',
      title: 'Graph mode with skip connections',
      mode: 'graph',
      stages: ['graph_adjacency.csv'],
      loc: 'graph_loc.csv',
      params: { n: 120, merge_frac: 0.06, balance: 'warn', node_label_pad: 0.03 },
      style: { colorBy: 'source', palette: 'okabe-ito', opacity: 0.78, gradient: true, gradientMode: 'merge' },
      files: {
        'graph_adjacency.csv': [
          '0,0,0.05,0.02,0,0', '0,0,0.03,0,0.04,0', '0,0,0,0.05,0,0.03',
          '0,0,0,0,0,0.07', '0,0,0,0,0,0.04', '0,0,0,0,0,0'
        ].join('\n') + '\n',
        'graph_loc.csv': [
          'set,node,x,y,label,cex,font',
          '1,1,0.0,0.72,Source A,0.8,2',
          '1,2,0.0,0.30,Source B,0.8,2',
          '2,1,1.3,0.55,Hub,0.8,2',
          '3,1,2.5,0.70,Relay,0.8,2',
          '3,2,2.5,0.26,Bypass,0.8,2',
          '4,1,3.7,0.50,Sink,0.8,2'
        ].join('\n') + '\n'
      }
    }
  ];
  if (typeof module !== 'undefined' && module.exports) module.exports = SK;
})(typeof window !== 'undefined' ? window : globalThis);
