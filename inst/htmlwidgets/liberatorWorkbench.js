(function () {
  "use strict";
  var e = React.createElement;
  function list(x) { return Array.isArray(x) ? x : []; }
  function value(x, fallback) { return x === undefined || x === null || x === "" ? fallback : x; }
  function number(x) { if (x === undefined || x === null || x === "") return null; var n = Number(x); return isFinite(n) ? n : null; }
  function fmt(x, digits) { var n = number(x); return n === null ? "—" : n.toFixed(digits === undefined ? 3 : digits).replace(/\.0+$/, ""); }
  function initialDarkTheme(legacyKey) {
    return window.LibeRDesign.theme.initialDark(legacyKey);
  }
  function storeTheme(dark, legacyKey) {
    window.LibeRDesign.theme.store(dark, legacyKey, false);
  }
  function useDialogFocus(onClose) {
    var dialog = React.useRef(null), close = React.useRef(onClose);
    close.current = onClose;
    React.useEffect(function () {
      var prior = document.activeElement, node = dialog.current;
      function items() { return node ? Array.prototype.slice.call(node.querySelectorAll('button:not([disabled]),input:not([disabled]),select:not([disabled]),textarea:not([disabled]),a[href],[tabindex]:not([tabindex="-1"])')) : []; }
      function keydown(event) {
        if (event.key === "Escape") { event.preventDefault(); close.current(); return; }
        if (event.key !== "Tab" || !node) return;
        var candidates = items();
        if (!candidates.length) { event.preventDefault(); node.focus(); return; }
        if (event.shiftKey && document.activeElement === candidates[0]) { event.preventDefault(); candidates[candidates.length - 1].focus(); }
        else if (!event.shiftKey && document.activeElement === candidates[candidates.length - 1]) { event.preventDefault(); candidates[0].focus(); }
      }
      document.addEventListener("keydown", keydown);
      window.setTimeout(function () { var candidates = items(); (candidates[0] || node).focus(); }, 0);
      return function () { document.removeEventListener("keydown", keydown); if (prior && prior.focus) prior.focus(); };
    }, []);
    return dialog;
  }
  function emit(props, action, detail) {
    if (!window.Shiny || !window.Shiny.setInputValue) return;
    window.Shiny.setInputValue((props.inputId || "liberator_workbench") + "_event",
      Object.assign({ action: action, nonce: Date.now() }, detail || {}), { priority: "event" });
  }
  function Button(props) {
    return e("button", { type: "button", className: "lr-button " + value(props.className, ""),
      disabled: !!props.disabled, title: props.title, "aria-label": props.ariaLabel || props.title, onClick: props.onClick },
      props.icon ? e("span", { className: "lr-button-icon", "aria-hidden": "true" }, props.icon) : null,
      props.children);
  }
  function MedicationAddButton(props) {
    return e(Button,{
      className:"lr-add-medication",disabled:props.disabled,
      title:"Add medication",ariaLabel:"Add medication",onClick:props.onClick
    },e("span",{className:"lr-add-medication-mark","aria-hidden":"true"}));
  }
  function Badge(props) { return e("span", { className: "lr-badge lr-badge-" + value(props.tone, "neutral") }, props.children); }
  function Empty(props) { return e("div", { className: "lr-empty" }, e("span", { className: "lr-empty-icon" }, value(props.icon, "◇")), e("strong", null, props.title), e("p", null, props.detail)); }
  function Panel(props) { return e("section", { className: "lr-panel " + value(props.className, "") },
    e("header", { className: "lr-panel-head" }, e("div", null, e("strong", null, props.title), props.subtitle ? e("span", null, props.subtitle) : null), props.actions || null),
    e("div", { className: "lr-panel-body" }, props.children)); }
  function Logo() { return e("span", { className: "lr-logo lr-logo-fallback", "aria-hidden": "true" }, "L"); }
  function ThemeSwitch(props) { return e("label", { className: "lr-theme-switch", title: "Switch colour theme" },
    e("span", null, props.dark ? "Dark" : "Light"), e("input", { type: "checkbox", checked: props.dark, onChange: props.onChange }), e("i", null)); }

  function Modal(props) { var dialog = useDialogFocus(props.onClose); return e("div", { className: "lr-modal-layer", role: "presentation", onMouseDown: function (x) { if (x.target === x.currentTarget) props.onClose(); } },
    e("section", { ref: dialog, tabIndex: -1, className: "lr-modal " + value(props.className, ""), role: "dialog", "aria-modal": "true", "aria-label": props.title },
      e("header", null, e("div", null, e("strong", null, props.title), props.subtitle ? e("span", null, props.subtitle) : null),
        e(Button, { className: "lr-icon-button", onClick: props.onClose, title: "Close", ariaLabel: "Close" }, "×")),
      e("div", { className: "lr-modal-body" }, props.children)));
  }
  function Field(props) { return e("label", { className: "lr-field " + value(props.className, "") }, e("span", null, props.label), props.children, props.help ? e("small", null, props.help) : null); }
  function NewPatientModal(props) {
    var id = React.useState(""), label = React.useState(""), study = React.useState("");
    return e(Modal, { title: "New pseudonymous patient", subtitle: "Direct identifiers deliberately stay outside LibeRator", onClose: props.onClose },
      e("div", { className: "lr-form-grid" }, e(Field, { label: "Patient pseudonym", className: "lr-span-2", help: "Use the identifier issued by your study or institution." }, e("input", { value: id[0], onChange: function(x){id[1](x.target.value);}, autoFocus: true })),
        e(Field, { label: "Non-identifying label" }, e("input", { value: label[0], onChange: function(x){label[1](x.target.value);} })),
        e(Field, { label: "Study id" }, e("input", { value: study[0], onChange: function(x){study[1](x.target.value);} }))),
      e("footer", { className: "lr-modal-actions" }, e(Button, { onClick: props.onClose }, "Cancel"), e(Button, { className: "lr-primary", disabled: !id[0].trim(), onClick: function(){emit(props.owner,"new_patient",{patient_id:id[0],label:label[0],study_id:study[0]});props.onClose();} }, "Create patient")));
  }
  function DeletePatientModal(props) {
    var confirmation=React.useState("");
    var patient=props.owner.patient||{};
    return e(Modal,{title:"Delete patient",subtitle:"This permanently removes the encrypted patient record and cannot be undone",onClose:props.onClose},
      e("div",{className:"lr-danger-callout"},e("strong",null,"Delete "+value(patient.label,patient.id)+"?"),e("p",null,"The patient timeline, treatment profiles, assessments and saved predictions will be removed. A minimal deletion event remains in the encrypted audit chain.")),
      e(Field,{label:'Type "YES" to confirm',className:"lr-span-2"},e("input",{value:confirmation[0],autoFocus:true,onChange:function(x){confirmation[1](x.target.value);}})),
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-danger",disabled:confirmation[0]!=="YES",onClick:function(){emit(props.owner,"delete_patient",{confirmation:confirmation[0]});props.onClose();}},"Delete patient")));
  }
  function MedicationModal(props) {
    var drug=React.useState(""),therapeuticClass=React.useState(""),monitoringAnalytes=React.useState(""),configureEndpoint=React.useState(!props.nested);
    var known=["carbamazepine","lamotrigine","levetiracetam","phenobarbital","phenytoin","sodium valproate","valproic acid","vancomycin","warfarin"];
    return e(Modal,{title:"Add medication",subtitle:"Add treatment context now; doses and measurements remain separate timeline evidence",onClose:props.onClose},
      e("div",{className:"lr-form-grid"},
        e(Field,{label:"Drug *",className:"lr-span-2",help:"Known drugs can prefill an editable typical endpoint in the next step."},e("input",{value:drug[0],list:"lr-known-medications",autoFocus:true,onChange:function(x){drug[1](x.target.value);}}),e("datalist",{id:"lr-known-medications"},known.map(function(name){return e("option",{key:name,value:name});}))),
        e(Field,{label:"Therapeutic class (optional)",className:"lr-span-2",help:"Used only to suggest an endpoint family when there is no exact drug preset."},e("input",{value:therapeuticClass[0],placeholder:"e.g. antiseizure or glycopeptide",onChange:function(x){therapeuticClass[1](x.target.value);}})),
        e(Field,{label:"TDM metabolites or related analytes (optional)",className:"lr-span-2",help:"Enter one analyte per line, or separate names with semicolons. Commas inside chemical names are preserved."},e("textarea",{value:monitoringAnalytes[0],placeholder:"e.g. carbamazepine-10,11-epoxide",onChange:function(x){monitoringAnalytes[1](x.target.value);}})),
        !props.nested?e("label",{className:"lr-check-row lr-span-2"},e("input",{type:"checkbox",checked:configureEndpoint[0],onChange:function(x){configureEndpoint[1](x.target.checked);}}),e("span",null,e("strong",null,"Configure therapeutic endpoint next"),e("small",null,"Open the endpoint library with this medication and any available drug-specific targets preselected."))):null),
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",disabled:!drug[0].trim(),onClick:function(){emit(props.owner,"add_medication",{drug:drug[0],therapeutic_class:therapeuticClass[0],monitoring_analytes:monitoringAnalytes[0],configure_endpoint:props.nested?false:configureEndpoint[0]});props.onClose();}},!props.nested&&configureEndpoint[0]?"Next: choose endpoint":"Add medication")));
  }
  function EventModal(props) {
    var type = props.kind, selectedMedication=list(props.owner.medications).filter(function(item){return item.key===props.owner.selectedDrug;})[0]||null;
    var defaults = { dose: ["", "mg"], concentration: [selectedMedication?selectedMedication.drug:"", "mg/L"], covariate: ["WT", "kg"], state_boundary: ["", ""] }[type] || ["", ""];
    var time = React.useState(""), name = React.useState(defaults[0]), val = React.useState(""), unit = React.useState(defaults[1]), missing = React.useState(""), route = React.useState("oral"), rate = React.useState("0"), dosingInterval=React.useState(""), steadyState=React.useState(false), therapeuticClass=React.useState(""),treatmentDrug=React.useState(selectedMedication?selectedMedication.drug:"");
    var medications=list(props.owner.medications),tdmOptions=[];
    medications.forEach(function(m){tdmOptions.push({key:m.key+"|||"+m.drug,drug:m.drug,analyte:m.drug,label:m.drug});String(value(m.monitoring_analytes,"")).split("|").filter(Boolean).forEach(function(analyte){tdmOptions.push({key:m.key+"|||"+analyte,drug:m.drug,analyte:analyte,label:analyte+" ("+m.drug+")"});});});
    React.useEffect(function(){var active=medications.filter(function(item){return item.key===props.owner.selectedDrug;})[0];if(active&&(type==="dose"||type==="concentration")){treatmentDrug[1](active.drug);name[1](active.drug);}},[props.owner.selectedDrug]);
    var title = {dose:"Record dose",concentration:"Record TDM sample",covariate:"Record covariate",state_boundary:"Mark patient-state boundary"}[type];
    return e(Modal, { title: title, subtitle: "New evidence is appended; earlier records are never overwritten", onClose: props.onClose },
      e("div", { className: "lr-form-grid" },
        e(Field, { label: "Timeline time (hours)" }, e("input", { type:"number",step:"any",value:time[0],onChange:function(x){time[1](x.target.value);},autoFocus:true })),
        type === "dose" ? e(Field,{label:"Drug"},e("div",{className:"lr-input-action"},e("select",{value:treatmentDrug[0],onChange:function(x){treatmentDrug[1](x.target.value);name[1](x.target.value);}},e("option",{value:""},"Select added medication"),medications.map(function(m){return e("option",{key:m.key,value:m.drug},m.drug);})),e(MedicationAddButton,{onClick:props.openMedication}))) : type === "concentration" ? e(Field,{label:"Drug or monitored analyte"},e("div",{className:"lr-input-action"},e("select",{value:treatmentDrug[0]+"|||"+name[0],onChange:function(x){var chosen=tdmOptions.filter(function(option){return option.key===x.target.value;})[0];if(chosen){treatmentDrug[1](chosen.drug);name[1](chosen.analyte);}}},e("option",{value:"|||"},"Select added medication or analyte"),tdmOptions.map(function(option){return e("option",{key:option.key,value:option.key},option.label);})),e(MedicationAddButton,{onClick:props.openMedication}))) : type !== "state_boundary" ? e(Field,{label:"Covariate"},e("input",{value:name[0],onChange:function(x){name[1](x.target.value);}})) : null,
        type !== "state_boundary" ? e(Field, { label: "Value" }, e("input", {type:"number",step:"any",value:val[0],onChange:function(x){val[1](x.target.value);}})) : null,
        type !== "state_boundary" ? e(Field, { label: "Unit" }, e("input", {value:unit[0],onChange:function(x){unit[1](x.target.value);}})) : null,
        type === "dose" ? e(Field, {label:"Route"}, e("select",{value:route[0],onChange:function(x){route[1](x.target.value);}},e("option",null,"oral"),e("option",null,"intravenous"),e("option",null,"other"))) : null,
        type === "dose" ? e(Field, {label:"Rate (amount/hour; 0 = bolus/oral)"}, e("input",{type:"number",step:"any",value:rate[0],onChange:function(x){rate[1](x.target.value);}})) : null,
        type === "dose" ? e(Field, {label:"Dosing interval (hours, optional)",help:"Used for the post-dose individualised PK profile and regimen context."}, e("input",{type:"number",min:"0.25",step:"0.25",value:dosingInterval[0],onChange:function(x){dosingInterval[1](x.target.value);}})) : null,
        type === "dose" ? e("label",{className:"lr-check-row lr-span-2"},e("input",{type:"checkbox",checked:steadyState[0],onChange:function(x){steadyState[1](x.target.checked);}}),e("span",null,e("strong",null,"Steady state before this dose"),e("small",null,"Use prior-dose accumulation for an established regular regimen. A dosing interval is required; leave this clear for a first or isolated dose."))) : null,
        type === "dose" ? e(Field, {label:"Therapeutic class (optional)",className:"lr-span-2",help:"Used to suggest an endpoint family when the exact drug is not in the reference presets."}, e("input",{value:therapeuticClass[0],placeholder:"e.g. antiseizure or glycopeptide",onChange:function(x){therapeuticClass[1](x.target.value);}})) : null,
        type !== "dose" && type !== "state_boundary" ? e(Field,{label:"Missing reason",className:"lr-span-2",help:"Required when a scheduled value is unavailable."},e("input",{value:missing[0],onChange:function(x){missing[1](x.target.value);}})) : null),
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",disabled:!time[0] || (type !== "state_boundary" && !name[0]) || (type === "dose" && steadyState[0] && !(Number(dosingInterval[0]) > 0)),onClick:function(){emit(props.owner,"add_event",{type:type,time:time[0],name:name[0],treatment_drug:treatmentDrug[0],value:val[0],unit:unit[0],missing_reason:missing[0],route:route[0],rate:rate[0],dosing_interval:dosingInterval[0],steady_state:steadyState[0],therapeutic_class:therapeuticClass[0]});props.onClose();}},"Add to timeline")));
  }
  function EvidenceCorrectionModal(props) {
    var original=props.event||{},metadata=original.metadata||{},type=original.type;
    var time=React.useState(String(value(original.time,""))),name=React.useState(value(original.name,"")),val=React.useState(original.value===null||original.value===undefined?"":String(original.value)),unit=React.useState(value(original.unit,"")),source=React.useState(value(original.source,"manual")),missing=React.useState(value(original.missing,"")),reason=React.useState(""),actor=React.useState("local-clinician"),entered=React.useState(false);
    var route=React.useState(value(metadata.route,"oral")),rate=React.useState(String(value(metadata.rate,0))),dosingInterval=React.useState(Number(metadata.ii)>0?String(metadata.ii):""),steadyState=React.useState(Number(metadata.ss)===1),treatmentDrug=React.useState(value(metadata.drug,original.name));
    var medications=list(props.owner.medications),tdmOptions=[];
    medications.forEach(function(m){tdmOptions.push({key:m.key+"|||"+m.drug,drug:m.drug,analyte:m.drug,label:m.drug});String(value(m.monitoring_analytes,"")).split("|").filter(Boolean).forEach(function(analyte){tdmOptions.push({key:m.key+"|||"+analyte,drug:m.drug,analyte:analyte,label:analyte+" ("+m.drug+")"});});});
    var replacementValid=String(time[0]).trim()!==""&&isFinite(Number(time[0]))&&(type==="state_boundary"||String(name[0]).trim()!=="")&&(type!=="dose"||Number(val[0])>0)&&(type==="dose"||type==="state_boundary"||String(val[0]).trim()!==""||String(missing[0]).trim()!=="")&&(!steadyState[0]||Number(dosingInterval[0])>0);
    var valid=String(reason[0]).trim()!==""&&String(actor[0]).trim()!==""&&(entered[0]||replacementValid);
    return e(Modal,{title:"Amend evidence",subtitle:"The original remains immutable; this action appends an auditable correction",onClose:props.onClose},
      e("div",{className:"lr-correction-original"},e("strong",null,"Original record"),e("span",null,value(type,"evidence")+" · "+fmt(original.time,2)+" h · "+value(original.name,"unnamed")),e("small",null,original.value===null||original.value===undefined?value(original.missing,"No value"):String(original.value)+" "+value(original.unit,""))),
      e("label",{className:"lr-check-row lr-span-2"},e("input",{type:"checkbox",checked:entered[0],onChange:function(x){entered[1](x.target.checked);}}),e("span",null,e("strong",null,"Mark as entered in error"),e("small",null,"Withdraw this event from modelling without creating replacement clinical evidence."))),
      !entered[0]?e("div",{className:"lr-form-grid"},
        e(Field,{label:"Timeline time (hours)"},e("input",{type:"number",step:"any",value:time[0],onChange:function(x){time[1](x.target.value);}})),
        type==="dose"?e(Field,{label:"Drug"},e("select",{value:treatmentDrug[0],onChange:function(x){treatmentDrug[1](x.target.value);name[1](x.target.value);}},medications.map(function(m){return e("option",{key:m.key,value:m.drug},m.drug);}))) : type==="concentration"?e(Field,{label:"Drug or monitored analyte"},e("select",{value:treatmentDrug[0]+"|||"+name[0],onChange:function(x){var chosen=tdmOptions.filter(function(option){return option.key===x.target.value;})[0];if(chosen){treatmentDrug[1](chosen.drug);name[1](chosen.analyte);}}},tdmOptions.map(function(option){return e("option",{key:option.key,value:option.key},option.label);}))) : type!=="state_boundary"?e(Field,{label:"Variable or event name"},e("input",{value:name[0],onChange:function(x){name[1](x.target.value);}})):null,
        type!=="state_boundary"?e(Field,{label:"Value"},e("input",{type:"number",step:"any",value:val[0],onChange:function(x){val[1](x.target.value);}})):null,
        type!=="state_boundary"?e(Field,{label:"Unit"},e("input",{value:unit[0],onChange:function(x){unit[1](x.target.value);}})):null,
        type==="dose"?e(Field,{label:"Route"},e("input",{value:route[0],onChange:function(x){route[1](x.target.value);}})):null,
        type==="dose"?e(Field,{label:"Rate (amount/hour; 0 = bolus/oral)"},e("input",{type:"number",step:"any",value:rate[0],onChange:function(x){rate[1](x.target.value);}})):null,
        type==="dose"?e(Field,{label:"Dosing interval (hours, optional)"},e("input",{type:"number",min:"0.25",step:"0.25",value:dosingInterval[0],onChange:function(x){dosingInterval[1](x.target.value);}})):null,
        type==="dose"?e("label",{className:"lr-check-row lr-span-2"},e("input",{type:"checkbox",checked:steadyState[0],onChange:function(x){steadyState[1](x.target.checked);}}),e("span",null,e("strong",null,"Steady state before this dose"),e("small",null,"A positive dosing interval is required."))):null,
        type!=="dose"&&type!=="state_boundary"?e(Field,{label:"Missing reason",className:"lr-span-2"},e("input",{value:missing[0],onChange:function(x){missing[1](x.target.value);}})):null,
        e(Field,{label:"Evidence source",className:"lr-span-2"},e("input",{value:source[0],onChange:function(x){source[1](x.target.value);}}))
      ):null,
      e("div",{className:"lr-form-grid lr-correction-governance"},
        e(Field,{label:"Correction reason *",className:"lr-span-2",help:"Explain what was wrong and why this replacement or withdrawal is appropriate."},e("textarea",{value:reason[0],autoFocus:true,onChange:function(x){reason[1](x.target.value);}})),
        e(Field,{label:"Recorded by *",className:"lr-span-2",help:"Use an institutional user identifier where available."},e("input",{value:actor[0],onChange:function(x){actor[1](x.target.value);}}))),
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:entered[0]?"lr-danger":"lr-primary",disabled:!valid,onClick:function(){emit(props.owner,"correct_event",{event_id:original.id,reason:reason[0],actor:actor[0],entered_in_error:entered[0],type:type,time:time[0],name:name[0],treatment_drug:treatmentDrug[0],value:val[0],unit:unit[0],source:source[0],missing_reason:missing[0],route:route[0],rate:rate[0],dosing_interval:dosingInterval[0],steady_state:steadyState[0]});props.onClose();}},entered[0]?"Mark entered in error":"Save correction")));
  }
  function RegimenModal(props) {
    var defaults=props.owner.regimenDefaults||{};
    var amounts=React.useState(list(defaults.amounts).length?list(defaults.amounts).join(", "):"100, 200, 300"), intervals=React.useState(list(defaults.intervals).length?list(defaults.intervals).join(", "):"12, 24"), horizon=React.useState(String(value(defaults.horizon,168))), nsim=React.useState(String(value(defaults.posteriorDraws,100))), residual=React.useState(false);
    return e(Modal,{title:"Explore candidate regimens",subtitle:"Rank feasible options against the selected endpoint",onClose:props.onClose},
      e("div",{className:"lr-form-grid"},e(Field,{label:"Dose amounts",help:"Comma-separated grid"},e("input",{value:amounts[0],onChange:function(x){amounts[1](x.target.value);}})),e(Field,{label:"Intervals (hours)"},e("input",{value:intervals[0],onChange:function(x){intervals[1](x.target.value);}})),e(Field,{label:"Evaluation horizon (hours)"},e("input",{type:"number",value:horizon[0],onChange:function(x){horizon[1](x.target.value);}})),e(Field,{label:"Conditional ETA draws",help:"Laplace approximation conditional on the selected population model and fitted population parameters."},e("input",{type:"number",min:10,value:nsim[0],onChange:function(x){nsim[1](x.target.value);}})),e(Field,{label:"Predictive variability",className:"lr-span-2",help:"When enabled, endpoint attainment and forecast intervals are evaluated on residualised observations (DV), not IPRED."},e("label",{className:"lr-check"},e("input",{type:"checkbox",checked:residual[0],onChange:function(x){residual[1](x.target.checked);}}),e("span",null,"Include residual observation variability")))),
      e("div",{className:"lr-callout"},"This comparison reports uncertainty and target attainment; LibeRator currently has Research validation status and does not issue an autonomous prescription."),
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",onClick:function(){emit(props.owner,"optimise",{amounts:amounts[0],intervals:intervals[0],horizon:horizon[0],nsim:nsim[0],residual:residual[0]});props.onClose();}},"Compare regimens")));
  }
  function AssessmentModal(props) {
    var mode=props.mode||"static",scope=React.useState("automatic"),count=React.useState("2"),since=React.useState(""),covmethod=React.useState("none"),covage=React.useState("720"),processScale=React.useState("0.1"),info=props.owner.profileObservation||{},readiness=props.owner.readiness||{},dynamic=mode==="dynamic";
    var valid=scope[0]!=="last_n"||Number(count[0])>=1;
    if(scope[0]==="since")valid=String(since[0]).trim()!==""&&isFinite(Number(since[0]));
    if(covmethod[0]==="locf")valid=valid&&Number(covage[0])>0&&isFinite(Number(covage[0]));
    if(dynamic)valid=valid&&Number(processScale[0])>0&&isFinite(Number(processScale[0]));
    return e(Modal,{title:dynamic?"Estimate parameters changing over time":"Estimate stable patient parameters",subtitle:dynamic?"Fit separate patient states across declared boundaries":"Fit one patient-specific parameter state across the available evidence",onClose:props.onClose},
      dynamic&&!readiness.dynamicReady?e("div",{className:"lr-callout lr-callout-warning"},value(readiness.dynamicReason,"At least two states with measured TDM evidence are required.")):null,
      e("div",{className:"lr-callout lr-callout-info"},e("strong",null,"Evidence used for estimation"),e("p",null,"All eligible patient evidence through the current timeline cutoff remains available to the conditional MAP fit. The option below controls which TDM observations are displayed over the individualised dosing-interval curve.")),
      e("div",{className:"lr-form-grid"},
        e(Field,{label:"TDM observations shown",className:"lr-span-2",help:"Automatic identifies the current monitoring episode from regimen changes, state boundaries, and gaps longer than six weeks."},
          e("select",{value:scope[0],onChange:function(x){scope[1](x.target.value);}},
            e("option",{value:"automatic"},"Current monitoring episode (recommended)"),
            e("option",{value:"latest"},"Most recent TDM only"),
            e("option",{value:"last_n"},"Most recent N TDM measurements"),
            e("option",{value:"all"},"All historical TDM measurements"),
            e("option",{value:"since"},"TDM measurements since a timeline hour"))),
        scope[0]==="last_n"?e(Field,{label:"Number of recent TDM measurements"},e("input",{type:"number",min:"1",step:"1",value:count[0],onChange:function(x){count[1](x.target.value);}})):null,
        scope[0]==="since"?e(Field,{label:"Starting timeline hour"},e("input",{type:"number",step:"any",value:since[0],onChange:function(x){since[1](x.target.value);}})):null,
        e(Field,{label:"Missing covariates",help:"No carry-forward is performed unless you explicitly select it."},e("select",{value:covmethod[0],onChange:function(x){covmethod[1](x.target.value);}},e("option",{value:"none"},"Require evidence at each model time"),e("option",{value:"locf"},"Carry last value forward (LOCF)"))),
        covmethod[0]==="locf"?e(Field,{label:"Maximum LOCF age (hours)",help:"Values older than this remain unresolved."},e("input",{type:"number",min:"0.01",step:"1",value:covage[0],onChange:function(x){covage[1](x.target.value);}})):null,
        dynamic?e(Field,{label:"Process variance scale",help:"Random-walk innovation covariance as a multiple of OMEGA; this clinical modelling choice is recorded with the assessment."},e("input",{type:"number",min:"0.000001",step:"0.01",value:processScale[0],onChange:function(x){processScale[1](x.target.value);}})):null),
      scope[0]==="automatic"?e("p",{className:"lr-profile-context"},value(info.automaticLabel,"Current monitoring episode")+"; "+value(info.totalCount,0)+" measured TDM point(s) are available in total."):null,
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",disabled:!valid||(dynamic&&!readiness.dynamicReady),onClick:function(){emit(props.owner,"assess",{mode:mode,process_scale:Number(processScale[0]),covariate_method:covmethod[0],covariate_max_age:Number(covage[0]),profile_observation_scope:scope[0],profile_observation_count:Number(count[0]),profile_observation_since:scope[0]==="since"?Number(since[0]):null});props.onClose();}},dynamic?"Run time-changing assessment":"Run stable assessment")));
  }
  function EndpointLibraryModal(props) {
    var rows=list(props.owner.endpointTemplates);
    var search=React.useState("");
    var edit=props.editMode?props.owner.endpointEdit:null;
    function defaultsFor(row){
      var defaults={};
      if(row)list(row.fields).forEach(function(field){defaults[field.name]=field.default===undefined||field.default===null?"":field.default;});
      return defaults;
    }
    var recommended=rows.filter(function(row){return row.recommended;})[0]||null;
    var editRow=edit?rows.filter(function(row){return row.id===edit.template_id;})[0]||null:null;
    var initial=editRow||recommended,initialValues=Object.assign(defaultsFor(initial),edit&&edit.values?edit.values:{});
    var selected=React.useState(initial),values=React.useState(initialValues);
    var visibleRows=rows.filter(function(row){return (row.name+" "+row.kind+" "+value(row.description,"")).toLowerCase().indexOf(search[0].toLowerCase())>=0;});
    function choose(row){
      selected[1](row);values[1](defaultsFor(row));
    }
    function update(name,next){values[1](Object.assign({},values[0],(function(){var item={};item[name]=next;return item;})()));}
    var fields=selected[0]?list(selected[0].fields):[];
    var complete=!!selected[0]&&fields.every(function(field){return !field.required||String(value(values[0][field.name],"")).trim()!=="";});
    function editor(field){
      var current=value(values[0][field.name],"");
      var locked=!!edit&&field.name==="drug";
      var control=field.type==="select"?
        e("select",{value:current,disabled:locked,onChange:function(x){update(field.name,x.target.value);}},list(field.options).map(function(option){return e("option",{key:option,value:option},option);})):
        field.type==="textarea"?
          e("textarea",{rows:3,value:current,disabled:locked,onChange:function(x){update(field.name,x.target.value);}}):
          e("input",{type:field.type==="number"?"number":"text",step:field.type==="number"?"any":undefined,value:current,disabled:locked,onChange:function(x){update(field.name,x.target.value);}});
      return e(Field,{key:field.name,label:field.label+(field.required?" *":""),help:field.help,className:Number(field.span)===2?"lr-span-2":""},control);
    }
    return e(Modal,{title:edit?"Modify therapeutic endpoint":"Therapeutic endpoint library",subtitle:edit?"Create a new patient–medication endpoint version while retaining the previous record":props.owner.selectedDrug?"Configure an endpoint for "+value((list(props.owner.medications).filter(function(x){return x.key===props.owner.selectedDrug;})[0]||{}).drug,props.owner.selectedDrug):"Versioned endpoint families; target values must come from an approved protocol",onClose:props.onClose},
      edit?e("div",{className:"lr-callout lr-preset-callout"},e("strong",null,"Editing v"+edit.original_version),e("span",null,"Current values are prefilled and remain unchanged in the audit history. Save with the suggested new version or choose another unused version.")):rows.length?e("div",null,
        e("div",{className:"lr-search lr-library-search"},e("span",null,"⌕"),e("input",{placeholder:"Search endpoint families",value:search[0],onChange:function(x){search[1](x.target.value);}})),
        e("div",{className:"lr-table-wrap lr-endpoint-library"},e("table",{className:"lr-table"},e("thead",null,e("tr",null,["Select","Endpoint","Metric family"].map(function(x){return e("th",{key:x},x);}))),e("tbody",null,visibleRows.map(function(row){var active=selected[0]&&selected[0].id===row.id;return e("tr",{key:row.id,className:"lr-endpoint-template-row"+(active?" lr-selected":""),tabIndex:0,role:"radio","aria-checked":!!active,onClick:function(){choose(row);},onKeyDown:function(x){if(x.key==="Enter"||x.key===" "){x.preventDefault();choose(row);}}},e("td",null,e("span",{className:"lr-radio"+(active?" active":"")},active?"\u2713":"")),e("td",null,e("strong",null,row.name),row.recommended?e(Badge,{tone:"covariate"},"Drug preset"):null,e("small",{className:"lr-source"},row.description)),e("td",null,row.kind));}))))):e("p",{className:"lr-muted"},"No endpoint templates are available."),
      selected[0]?e("section",{className:"lr-endpoint-template-form"},e("h3",null,(edit?"Revise ":"Configure ")+selected[0].name),!edit&&selected[0].preset_label?e("div",{className:"lr-callout lr-preset-callout"},e("strong",null,selected[0].preset_label),e("span",null,"Drug-specific reference values have been prefilled. Review and edit them for the indication, individual target, assay and institutional policy.")):null,e("p",{className:"lr-muted"},"Required values are not universal clinical targets. Review every value and its evidence before registration."),e("div",{className:"lr-form-grid"},fields.map(editor))):e("div",{className:"lr-callout"},"Select an endpoint family to configure its targets, units, evidence source, governance status, and version."),
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",disabled:!complete,onClick:function(){emit(props.owner,edit?"revise_endpoint":"create_endpoint",{template_id:selected[0].id,values:values[0],original_key:edit?edit.original_key:""});props.onClose();}},edit?"Save new endpoint version":"Add & select endpoint")));
  }

  function EndpointSetModal(props) {
    var owner=props.owner,edit=props.editMode&&owner.endpointEdit&&owner.endpointEdit.kind==="multi_endpoint"?owner.endpointEdit:null;
    var available=list(owner.endpoints).filter(function(item){return item.drugKey===owner.selectedDrug&&!item.isSet;});
    var initial={};
    if(edit)list(edit.components).forEach(function(component){if(component.endpointKey)initial[component.endpointKey]={role:component.role,weight:String(value(component.weight,1)),hard:!!component.hardConstraint,minimum:String(value(component.minimumAttainment,.9))};});
    var selected=React.useState(initial),name=React.useState(edit?edit.name:"Combined clinical objective"),source=React.useState(edit?edit.source:""),status=React.useState(edit?edit.status:"draft"),version=React.useState(edit?edit.version:"1.0.0");
    function update(key,patch){selected[1](function(current){var next=Object.assign({},current),row=Object.assign({},next[key]||{},patch);next[key]=row;return next;});}
    function toggle(row){
      selected[1](function(current){
        var next=Object.assign({},current);
        if(next[row.id])delete next[row.id];
        else next[row.id]={role:Object.keys(next).length?"secondary":"primary",weight:"1",hard:false,minimum:".9"};
        return next;
      });
    }
    var keys=Object.keys(selected[0]),primary=keys.filter(function(key){return selected[0][key].role==="primary";});
    var complete=keys.length>=2&&primary.length===1&&name[0].trim()&&source[0].trim()&&version[0].trim()&&keys.every(function(key){var item=selected[0][key];return Number(item.weight)>0&&(!item.hard||(Number(item.minimum)>=0&&Number(item.minimum)<=1));});
    function save(){
      var components=keys.map(function(key){var item=selected[0][key];return{endpoint_key:key,role:item.role,weight:Number(item.weight),hard_constraint:!!item.hard,minimum_attainment:Number(item.minimum)};});
      emit(owner,edit?"revise_endpoint_set":"create_endpoint_set",{name:name[0],source:source[0],status:status[0],version:version[0],components:components,original_key:edit?edit.original_key:""});
      props.onClose();
    }
    return e(Modal,{className:"lr-modal-wide",title:edit?"Modify multi-endpoint objective":"Combine therapeutic endpoints",subtitle:"Define one explicit primary objective, optional secondary objectives, and conditional-draw safety constraints",onClose:props.onClose},
      edit?e("div",{className:"lr-callout lr-preset-callout"},e("strong",null,"Editing v"+edit.original_version),e("span",null,"Saving creates a new immutable patient\u2013medication objective version.")):null,
      e("div",{className:"lr-form-grid"},
        e(Field,{label:"Objective name *"},e("input",{value:name[0],onChange:function(x){name[1](x.target.value);}})),
        e(Field,{label:"Version *"},e("input",{value:version[0],onChange:function(x){version[1](x.target.value);}})),
        e(Field,{label:"Governance status",help:"Qualified status requires a separately attested governance record."},e("select",{value:status[0],onChange:function(x){status[1](x.target.value);}},["draft","reviewed"].map(function(item){return e("option",{key:item,value:item},item);}))),
        e(Field,{label:"Clinical source or protocol *"},e("input",{value:source[0],onChange:function(x){source[1](x.target.value);}}))),
      e("div",{className:"lr-callout lr-span-2"},e("strong",null,"Decision rule"),e("p",null,"Hard constraints are applied first. Eligible regimens are ranked by expected normalized utility and marked when they lie on the endpoint-wise Pareto frontier. Weights are relative and always remain visible.")),
      available.length?e("div",{className:"lr-table-wrap lr-endpoint-set-table"},e("table",{className:"lr-table"},e("thead",null,e("tr",null,["Use","Endpoint","Role","Weight","Hard constraint","Minimum attainment"].map(function(label){return e("th",{key:label},label);}))),e("tbody",null,available.map(function(row){var item=selected[0][row.id],active=!!item;return e("tr",{key:row.id,className:active?"lr-selected":""},
        e("td",null,e("input",{type:"checkbox",checked:active,onChange:function(){toggle(row);}})),
        e("td",null,e("strong",null,row.name),e("small",{className:"lr-source"},row.kind+" \u00b7 v"+row.version)),
        e("td",null,e("select",{disabled:!active,value:active?item.role:"secondary",onChange:function(x){var role=x.target.value;update(row.id,{role:role,hard:role==="safety"?true:item.hard});}},["primary","secondary","safety"].map(function(role){return e("option",{key:role,value:role},role);}))),
        e("td",null,e("input",{type:"number",min:".001",step:".1",disabled:!active,value:active?item.weight:"1",onChange:function(x){update(row.id,{weight:x.target.value});}})),
        e("td",null,e("input",{type:"checkbox",disabled:!active,checked:active&&item.hard,onChange:function(x){update(row.id,{hard:x.target.checked});}})),
        e("td",null,e("input",{type:"number",min:"0",max:"1",step:".01",disabled:!active||!item.hard,value:active?item.minimum:".9",onChange:function(x){update(row.id,{minimum:x.target.value});}}))
      );})))):e("div",{className:"lr-callout lr-callout-warning"},"Create at least two endpoint definitions for this medication before combining them."),
      keys.length&&primary.length!==1?e("div",{className:"lr-inline-warning"},"Choose exactly one primary endpoint."):null,
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",disabled:!complete,onClick:save},edit?"Save new objective version":"Add & select objective")));
  }

  function ModelLibraryModal(props) {
    var owner=props.owner,medication=list(owner.medications).filter(function(item){return item.key===owner.selectedDrug;})[0]||null;
    var mode=React.useState("library"),selectedId=React.useState(""),acknowledge=React.useState(false);
    var templateId=React.useState("standard_advan"),name=React.useState((medication?medication.drug:"Patient")+" population model");
    var advan=React.useState("2"),trans=React.useState("2"),states=React.useState("2"),iiv=React.useState(true),residual=React.useState("proportional"),nTransit=React.useState("3");
    var libraryModels=list(owner.libraryModels),templates=list(owner.modelTemplates),selected=libraryModels.filter(function(item){return item.id===selectedId[0];})[0]||null;
    var template=templates.filter(function(item){return item.id===templateId[0];})[0]||templates[0]||null;
    var odeAdvan=["6","8","9","13","14"].indexOf(String(advan[0]))>=0;
    React.useEffect(function(){if(owner.modelLibraryAvailable)emit(owner,"load_model_library",{});},[owner.selectedDrug,owner.modelLibraryAvailable]);
    function chooseModel(item){selectedId[1](item.id);acknowledge[1](false);}
    function importModel(){emit(owner,"model_import_library",{library_id:selected.id,acknowledge_research:acknowledge[0]});props.onClose();}
    function createModel(){emit(owner,"model_create_template",{template_type:template&&template.type==="advan"?"advan":"structural",template_id:template?template.id:"",name:name[0],advan:advan[0],trans:trans[0],n_state:states[0],iiv:iiv[0],residual:residual[0],n_transit:nTransit[0]});props.onClose();}
    return e(Modal,{className:"lr-model-modal",title:"Add or select population model",subtitle:medication?"Models are scoped to "+medication.drug:"Select a medication first",onClose:props.onClose},
      e("div",{className:"lr-model-source-tabs",role:"tablist"},
        e("button",{type:"button",className:mode[0]==="library"?"active":"",role:"tab","aria-selected":mode[0]==="library",onClick:function(){mode[1]("library");}},"Select from LibeRary"),
        e("button",{type:"button",className:mode[0]==="template"?"active":"",role:"tab","aria-selected":mode[0]==="template",onClick:function(){mode[1]("template");}},"Create from LibeRation template")),
      mode[0]==="library"?e("div",{className:"lr-model-library-panel"},
        !owner.modelLibraryAvailable?e("div",{className:"lr-callout"},value(owner.modelLibraryReason,"The LibeRary catalogue is unavailable.")):
        !owner.modelLibraryLoaded?e("div",{className:"lr-model-library-loading"},e("i",null),e("span",null,"Loading medication-specific LibeRary models...")):
        libraryModels.length?e("div",{className:"lr-model-library-list",role:"radiogroup"},libraryModels.map(function(item){var active=item.id===selectedId[0];return e("button",{type:"button",key:item.id,className:"lr-model-library-card"+(active?" active":""),role:"radio","aria-checked":active,onClick:function(){chooseModel(item);}},
          e("span",{className:"lr-radio"+(active?" active":"")},active?"\u2713":""),
          e("span",{className:"lr-model-library-copy"},e("strong",null,item.title),e("small",null,value(item.population,"Population not recorded")),e("span",null,"ADVAN"+item.advan+(item.trans?" / TRANS"+item.trans:"")+" \u00b7 v"+value(item.version,"?"))),
          e("span",{className:"lr-model-library-badges"},item.clinicallyQualified?e(Badge,{tone:"covariate"},"Clinically qualified"):e(Badge,{tone:"warning"},value(item.status,"Research")),item.confidence!==null&&item.confidence!==undefined?e("small",null,Math.round(Number(item.confidence)*100)+"% confidence"):null));})):
        e(Empty,{title:"No medication-specific models",detail:"LibeRary has no usable model whose recorded compound matches "+value(medication&&medication.drug,"the selected medication")+". Create a research model from a LibeRation template instead."}),
        selected&&selected.researchAcknowledgementRequired?e("label",{className:"lr-check-row lr-model-research-ack"},e("input",{type:"checkbox",checked:acknowledge[0],onChange:function(x){acknowledge[1](x.target.checked);}}),e("span",null,e("strong",null,"Use review-stage model for Research work"),e("small",null,"This catalogue entry is not validated. Confirm that its source, assumptions, population and fit are suitable before using its output."))):null,
        e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",disabled:!selected||(selected.researchAcknowledgementRequired&&!acknowledge[0]),onClick:importModel},"Add & select model"))):
      e("div",{className:"lr-model-template-panel"},
        e("div",{className:"lr-callout"},e("strong",null,"One template implementation"),e("p",null,"This creates the same editable model object as LibeRation. It is a Research starting point, not a qualified medication model.")),
        e("div",{className:"lr-form-grid"},
          e(Field,{label:"Model name",className:"lr-span-2"},e("input",{value:name[0],autoFocus:true,onChange:function(x){name[1](x.target.value);}})),
          e(Field,{label:"LibeRation template",className:"lr-span-2"},e("select",{value:templateId[0],onChange:function(x){templateId[1](x.target.value);}},templates.map(function(item){return e("option",{key:item.id,value:item.id},item.name);})),template?e("small",null,template.notes):null),
          template&&template.type==="advan"?e(Field,{label:"ADVAN"},e("select",{value:advan[0],onChange:function(x){advan[1](x.target.value);}},Array.from({length:14},function(_,index){var value=String(index+1);return e("option",{key:value,value:value},"ADVAN"+value);}))):null,
          template&&template.type==="advan"&&!odeAdvan?e(Field,{label:"TRANS"},e("select",{value:trans[0],onChange:function(x){trans[1](x.target.value);}},Array.from({length:6},function(_,index){var value=String(index+1);return e("option",{key:value,value:value},"TRANS"+value);}))):null,
          template&&template.type==="advan"&&odeAdvan?e(Field,{label:"Number of states"},e("input",{type:"number",min:"1",max:"20",value:states[0],onChange:function(x){states[1](x.target.value);}})):null,
          template&&template.type==="structural"?e(Field,{label:"Residual model"},e("select",{value:residual[0],onChange:function(x){residual[1](x.target.value);}},["proportional","additive","combined","lognormal","none"].map(function(item){return e("option",{key:item,value:item},item);}))):null,
          template&&template.id==="transit_absorption"?e(Field,{label:"Transit compartments"},e("input",{type:"number",min:"1",max:"20",value:nTransit[0],onChange:function(x){nTransit[1](x.target.value);}})):null,
          template&&template.type==="structural"?e("label",{className:"lr-check-row lr-span-2"},e("input",{type:"checkbox",checked:iiv[0],onChange:function(x){iiv[1](x.target.checked);}}),e("span",null,e("strong",null,"Include inter-individual variability"),e("small",null,"Generate log-normal ETA terms for template parameters."))):null),
        e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",disabled:!name[0].trim()||!template,onClick:createModel},"Create & select model"))));
  }

  function ModelInfoModal(props) {
    var info=props.owner.selectedModelInfo;
    if(!info)return e(Modal,{title:"Population model information",onClose:props.onClose},e(Empty,{title:"No model selected",detail:"Select a population model to inspect its structure, parameters, covariates, provenance and limitations."}));
    var parameters=list(info.parameters),covariates=list(info.covariates),derived=list(info.derived),limitations=list(info.limitations);
    return e(Modal,{className:"lr-model-modal",title:info.name,subtitle:"Deterministic model summary and applicability review",onClose:props.onClose},
      e("div",{className:"lr-model-info-status"},e(Badge,{tone:info.qualificationStatus==="qualified"?"covariate":"warning"},value(info.qualificationStatus,"Research")),e("span",null,value(info.source,"Local model")),info.libraryId?e("span",null,info.libraryId+" · v"+value(info.libraryVersion,"?")):null),
      e("section",{className:"lr-model-info-section"},e("h3",null,"Model overview"),e("p",null,info.structure),info.population?e("div",{className:"lr-model-info-meta"},e("strong",null,"Recorded study population"),e("span",null,info.population)):null,info.route?e("div",{className:"lr-model-info-meta"},e("strong",null,"Route"),e("span",null,info.route)):null,info.confidence!==null&&info.confidence!==undefined?e("div",{className:"lr-model-info-meta"},e("strong",null,"Extraction confidence"),e("span",null,Math.round(Number(info.confidence)*100)+"%")):null),
      e("section",{className:"lr-model-info-section"},e("h3",null,"Population parameters"),parameters.length?e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table lr-model-parameter-table"},e("thead",null,e("tr",null,["Type","Parameter","Value","Meaning"].map(function(label){return e("th",{key:label},label);}))),e("tbody",null,parameters.map(function(item,index){return e("tr",{key:item.group+"-"+item.name+"-"+index},e("td",null,item.group),e("td",null,item.name),e("td",null,item.value===null||item.value===undefined?"—":fmt(item.value,6)),e("td",null,item.description));})))):e("p",{className:"lr-muted"},"No population parameter metadata is available.")),
      e("section",{className:"lr-model-info-section"},e("h3",null,"Required covariates"),covariates.length?e("div",{className:"lr-model-covariate-grid"},covariates.map(function(item){return e("div",{key:item.name},e("strong",null,item.name),e("span",null,item.description),list(item.usedIn).length?e("small",null,"Used in "+list(item.usedIn).join(", ")):null);})):e("p",{className:"lr-muted"},"This model declares no required covariates.")),
      derived.length?e("section",{className:"lr-model-info-section"},e("h3",null,"Derived model quantities"),e("p",{className:"lr-muted"},"These are deterministic calculations, not separately fitted parameters."),e("div",{className:"lr-model-derived-list"},derived.map(function(item){return e("div",{key:item.name},e("strong",null,item.name),e("span",null,item.description),item.expression?e("code",null,item.name+" = "+item.expression):null);}))):null,
      e("section",{className:"lr-model-info-section lr-model-limitations"},e("h3",null,"Limitations and applicability flags"),limitations.length?e("ul",null,limitations.map(function(item,index){return e("li",{key:index},item);})):e("p",{className:"lr-muted"},"No explicit limitations were recorded or triggered by the deterministic checks."),e("div",{className:"lr-callout"},e("strong",null,"How this section is generated"),e("p",null,"Flags come from catalogue status, qualification records, translation metadata, study scope and required model covariates. No language model is used, and absence of a flag is not evidence of clinical suitability."))),
      e("footer",{className:"lr-modal-actions"},e(Button,{className:"lr-primary",onClick:props.onClose},"Close")));
  }

  function Timeline(props) {
    var events=list(props.events), numeric=events.filter(function(x){return number(x.time)!==null;}), concentrations=numeric.filter(function(x){return x.type==="concentration"&&number(x.value)!==null;}), doses=numeric.filter(function(x){return x.type==="dose";}), boundaries=numeric.filter(function(x){return x.type==="state_boundary";});
    if (!numeric.length) return e(Empty,{icon:"⌁",title:"No longitudinal evidence yet",detail:"Record a dose, TDM sample, covariate, or state boundary to begin."});
    var times=numeric.map(function(x){return Number(x.time);}), minT=Math.min.apply(null,times), maxT=Math.max.apply(null,times); if(maxT===minT)maxT=minT+1;
    var vals=concentrations.map(function(x){return Number(x.value);}), maxV=vals.length?Math.max.apply(null,vals)*1.15:1;
    function x(t){return 58+(Number(t)-minT)/(maxT-minT)*810;} function y(v){return 245-Number(v)/maxV*180;}
    var line=concentrations.slice().sort(function(a,b){return a.time-b.time;}).map(function(p,i){return (i?"L":"M")+x(p.time)+" "+y(p.value);}).join(" ");
    return e("div",{className:"lr-timeline"},e("svg",{viewBox:"0 0 920 290",role:"img","aria-label":"Patient concentration timeline"},
      e("line",{x1:58,y1:245,x2:880,y2:245,className:"lr-axis"}),e("line",{x1:58,y1:45,x2:58,y2:245,className:"lr-axis"}),
      line?e("path",{d:line,className:"lr-conc-line"}):null,
      boundaries.map(function(p,i){return e("g",{key:"b"+i},e("line",{x1:x(p.time),y1:38,x2:x(p.time),y2:250,className:"lr-boundary"}),e("text",{x:x(p.time)+5,y:52,className:"lr-chart-label"},"state "+(i+2)));}),
      doses.map(function(p,i){return e("g",{key:"d"+i},e("line",{x1:x(p.time),y1:245,x2:x(p.time),y2:220,className:"lr-dose-line"}),e("path",{d:"M"+(x(p.time)-5)+" 220 L"+x(p.time)+" 211 L"+(x(p.time)+5)+" 220 Z",className:"lr-dose"}));}),
      concentrations.map(function(p,i){return e("g",{key:"c"+i},e("circle",{cx:x(p.time),cy:y(p.value),r:5,className:"lr-point"}),e("title",null,p.name+": "+p.value+" "+p.unit+" at "+p.time+" h"));}),
      e("text",{x:468,y:280,className:"lr-axis-title"},"Patient timeline (hours)"),e("text",{x:17,y:155,transform:"rotate(-90 17 155)",className:"lr-axis-title"},"Concentration"),
      e("text",{x:58,y:264,className:"lr-chart-label"},fmt(minT,1)),e("text",{x:868,y:264,className:"lr-chart-label"},fmt(maxT,1))),
      e("div",{className:"lr-legend"},e("span",null,e("i",{className:"lr-dot purple"}),"TDM concentration"),e("span",null,e("i",{className:"lr-triangle"}),"Dose"),e("span",null,e("i",{className:"lr-dash"}),"Latent-state boundary")));
  }
  function EvidenceTable(props) {
    var rows=list(props.eventLedger).slice().sort(function(a,b){return String(b.recordedAt).localeCompare(String(a.recordedAt));});
    if(!rows.length)return e(Empty,{title:"No evidence",detail:"The immutable evidence ledger is empty."});
    function statusTone(row){return row.status==="entered_in_error"||row.status==="superseded"?"warning":row.status==="corrected"?"concentration":"covariate";}
    return e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table lr-evidence-ledger"},e("thead",null,e("tr",null,["Clinical time","Status","Type","Variable","Value","Recorded","Source",""].map(function(x,i){return e("th",{key:x||"action-"+i},x);}))),e("tbody",null,rows.map(function(r){return e("tr",{key:r.id,className:r.active?"":"lr-evidence-superseded"},
      e("td",null,fmt(r.time,2)+" h"),
      e("td",null,e(Badge,{tone:statusTone(r)},String(value(r.status,"active")).replace(/_/g," "))),
      e("td",null,e(Badge,{tone:r.missing?"warning":r.type},r.type)),
      e("td",null,value(r.name,"—")),
      e("td",null,r.type==="correction"?e("span",{className:"lr-missing"},"No replacement evidence"):r.type==="state_boundary"?"—":r.value===null||r.value===undefined?e("span",{className:"lr-missing"},value(r.missing,"missing")):r.value+" "+value(r.unit,"")),
      e("td",null,value(r.recordedAt,"—")),
      e("td",null,value(r.source,"manual"),r.correctionReason?e("small",null,"Reason: "+r.correctionReason):null),
      e("td",null,r.amendable?e(Button,{className:"lr-small",onClick:function(){props.onCorrect(r);}},"Amend"):null));}))));
  }
  function ParameterTable(props) {
    var rows=list(props.current&&props.current.eta),outputs=list(props.current&&props.current.individualParameters),parameters=outputs.filter(function(item){return value(item.kind,"parameter")==="parameter";}),derived=outputs.filter(function(item){return item.kind==="derived";});
    if(!rows.length&&!outputs.length)return e(Empty,{title:"No individual estimate",detail:"Run a static or dynamic conditional MAP assessment after adding TDM observations."});
    function parameterTable(items){
      return e("div",null,e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table"},
        e("thead",null,e("tr",null,e("th",null,"State"),e("th",null,"Parameter"),e("th",null,"Value"),e("th",null,"At time"))),
        e("tbody",null,items.map(function(r,i){return e("tr",{key:i},e("td",null,"Occasion "+value(r.occasion,1)),e("td",null,r.parameter+(r.individualised===true?" *":"")),e("td",null,fmt(r.value,5)),e("td",null,fmt(r.time,2)+" h"));})))),
        e("p",{className:"lr-parameter-footnote"},"* Incorporates an estimated ETA and was individualised from this patient's data. Unmarked parameters retain their population/covariate-derived value."));
    }
    function derivedTable(items){
      return e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table"},
        e("thead",null,e("tr",null,e("th",null,"State"),e("th",null,"Quantity"),e("th",null,"Value"),e("th",null,"Definition"))),
        e("tbody",null,items.map(function(r,i){return e("tr",{key:i},e("td",null,"Occasion "+value(r.occasion,1)),e("td",null,r.parameter),e("td",null,fmt(r.value,5)),e("td",{className:"lr-expression-cell"},value(r.expression,"Calculated by model")));}))));
    }
    return e("div",{className:"lr-individual-tables"},
      e("section",null,e("h3",null,"Individual random effects (ETAs)"),rows.length?e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table"},e("thead",null,e("tr",null,e("th",null,"State"),e("th",null,"ETA"),e("th",null,"MAP estimate"),e("th",null,"Conditional SE"))),e("tbody",null,rows.map(function(r,i){return e("tr",{key:i},e("td",null,"Occasion "+value(r.occasion,1)),e("td",null,r.parameter),e("td",null,fmt(r.estimate,4)),e("td",null,fmt(r.standard_error,4)));})))):e("p",{className:"lr-muted"},"No ETA terms in this model.")),
      e("section",null,e("h3",null,"Individualised PK/PD parameters"),parameters.length?parameterTable(parameters):e("p",{className:"lr-muted"},"No structural parameter outputs were exposed by this model.")),
      derived.length?e("section",{className:"lr-derived-output-section"},e("h3",null,"Derived model quantities"),e("p",{className:"lr-muted"},"Calculated inputs and intermediate quantities are shown separately; they are not fitted PK/PD parameters."),derivedTable(derived)):null);
  }
  function IndividualProfile(props) {
    var all=list(props.current&&props.current.profile),rows=all.filter(function(r){return number(r.time)!==null&&number(r.ipred)!==null;}).sort(function(a,b){return a.time-b.time;}),observations=all.filter(function(r){return number(r.time)!==null&&number(r.observation)!==null;});
    if(!rows.length)return null;
    var interval=props.current&&props.current.profileInterval||{},width=920,height=330,left=62,right=24,top=25,bottom=48,times=rows.map(function(r){return Number(r.time);}),values=rows.map(function(r){return Number(r.ipred);});
    var summary=interval.summary||{},observationSelection=interval.observation_selection||{},meanOnly=interval.profile_type==="steady_state_mean_only",legacy=interval.profile_type==="legacy_event_only",population=interval.population_interval||{},populationRows=rows.filter(function(r){return number(r.populationLower)!==null&&number(r.populationMedian)!==null&&number(r.populationUpper)!==null;}),endpoint=list(props.endpoints).filter(function(item){return item.id===props.selectedEndpoint;})[0]||null,target=endpoint&&number(endpoint.lower)!==null&&number(endpoint.upper)!==null?endpoint:null;
    function metric(label,item,suffix){return e("div",null,e("span",null,label),e("strong",null,fmt(item&&item.median!==undefined?item.median:item,2)+(suffix||"")));}
    var metrics=e("div",{className:"lr-profile-metrics"},
      metric("Time-weighted mean",summary.mean_css),
      !meanOnly?metric("Trough",summary.trough):null,
      !meanOnly?metric("Peak",summary.peak):null,
      !meanOnly?metric("Peak-trough fluctuation",summary.fluctuation_percent,"%"):null);
    if(legacy)return e("section",{className:"lr-individual-profile"},
      e("h3",null,"Individualised PK profile unavailable"),
      e("div",{className:"lr-callout lr-callout-warning"},"This saved assessment was produced by an older worker and contains event-time predictions only. Run a new individual assessment to calculate the full post-dose curve and 95% similar-patient prediction interval."));
    if(meanOnly)return e("section",{className:"lr-individual-profile"},
      e("h3",null,"Individualised steady-state exposure"),
      e("p",{className:"lr-profile-context"},"This source model defines average steady-state concentration only; its structure contains no distribution volume or absorption term from which to infer a within-interval curve."),
      metrics,
      number(population.lower)!==null?e("p",{className:"lr-profile-context"},"95% similar-patient prediction interval (ETA variability only): "+fmt(population.lower,2)+" – "+fmt(population.upper,2)):null,
      e("div",{className:"lr-callout lr-callout-info"},"The reported value is not a simulated peak-to-trough profile. Peak, trough and transition kinetics are unavailable for this model."));
    populationRows.forEach(function(r){values.push(Number(r.populationLower),Number(r.populationUpper));});
    if(target)values.push(Number(target.lower),Number(target.upper));
    observations.forEach(function(r){values.push(Number(r.observation));});
    var minT=Math.min.apply(null,times),maxT=Math.max.apply(null,times);if(maxT===minT)maxT=minT+1;
    var minY=Math.min.apply(null,values),maxY=Math.max.apply(null,values),pad=(maxY-minY)*.12||1;minY=Math.max(0,minY-pad);maxY+=pad;
    function x(v){return left+(Number(v)-minT)/(maxT-minT)*(width-left-right);}function y(v){return top+(maxY-Number(v))/(maxY-minY)*(height-top-bottom);}
    var path=rows.map(function(r,i){return (i?"L":"M")+x(r.time)+" "+y(r.ipred);}).join(" "),populationUpper=populationRows.map(function(r,i){return(i?"L":"M")+x(r.time)+" "+y(r.populationUpper);}).join(" "),populationLower=populationRows.map(function(r,i){return(i?"L":"M")+x(r.time)+" "+y(r.populationLower);}).join(" ");
    return e("section",{className:"lr-individual-profile"},
      e("h3",null,"Individualised post-dose PK profile"),
      e("p",{className:"lr-profile-context"},value(interval.hours,fmt(maxT-minT,2))+" h interval · "+value(interval.grid_step_hours,.25)+" h prediction grid · "+value(interval.source,"assessment timeline")+(interval.assumed?" (assumed)":"")),
      e("p",{className:"lr-profile-context"},value(observationSelection.label,observations.length+" TDM observation(s)")+" shown; all eligible evidence was retained in the fit."),
      metrics,
      e("svg",{viewBox:"0 0 "+width+" "+height,role:"img","aria-label":"Individualised dense post-dose PK prediction, similar-patient prediction interval, and observed concentrations"},
        target?e("rect",{x:left,y:y(target.upper),width:width-left-right,height:Math.max(1,y(target.lower)-y(target.upper)),className:"lr-target-band"}):null,
        e("line",{x1:left,y1:height-bottom,x2:width-right,y2:height-bottom,className:"lr-axis"}),
        e("line",{x1:left,y1:top,x2:left,y2:height-bottom,className:"lr-axis"}),
        populationRows.length?e("path",{d:populationUpper,className:"lr-population-limit"}):null,
        populationRows.length?e("path",{d:populationLower,className:"lr-population-limit"}):null,
        e("path",{d:path,className:"lr-forecast-line"}),
        observations.map(function(r,i){return e("g",{key:i},e("circle",{cx:x(r.time),cy:y(r.observation),r:5,className:"lr-point"}),e("title",null,"TDM "+fmt(r.observation,2)+" at "+fmt(Number(r.time)-minT,2)+" h post-dose"+(number(r.observedTime)!==null?" (timeline hour "+fmt(r.observedTime,2)+")":"")));}),
        e("text",{x:width/2,y:height-8,textAnchor:"middle",className:"lr-axis-title"},"Time since dose (hours)"),
        e("text",{x:16,y:height/2,transform:"rotate(-90 16 "+(height/2)+")",textAnchor:"middle",className:"lr-axis-title"},"Concentration"),
        e("text",{x:left,y:height-26,className:"lr-chart-label"},"0"),
        e("text",{x:width-right-20,y:height-26,className:"lr-chart-label"},fmt(maxT-minT,1)),
        e("text",{x:left-9,y:y(minY)+3,textAnchor:"end",className:"lr-chart-label"},fmt(minY,2)),
        e("text",{x:left-9,y:y(maxY)+3,textAnchor:"end",className:"lr-chart-label"},fmt(maxY,2))),
      e("div",{className:"lr-legend"},
        e("span",null,e("i",{className:"lr-forecast-line-key"}),"Individual prediction"),
        populationRows.length?e("span",null,e("i",{className:"lr-population-key"}),"Dashed limits: 95% similar-patient prediction interval (ETA variability)"):null,
        target?e("span",null,e("i",{className:"lr-target-key"}),"Therapeutic range"):null,
        e("span",null,e("i",{className:"lr-dot"}),"Observation")));
  }
  function TaskProgress(props) {
    return e("div",{className:"lr-task-progress",role:"status","aria-live":"polite"},e("i",null),e("strong",null,props.title),e("p",null,props.detail),props.cancellable?e(Button,{onClick:props.cancel},"Cancel"):null);
  }
  function RegimenTable(props) {
    var rows=list(props.regimen&&props.regimen.summary);
    var task=props.task||{},optimising=task.running&&String(task.label).indexOf("Candidate regimen comparison")>=0;
    if(optimising)return e(TaskProgress,{title:"Exploring candidate regimens",detail:"Simulating and ranking the requested dose and interval combinations. Results will replace this message when ready.",cancellable:task.cancellable,cancel:function(){emit(props,"cancel_task",{id:task.id});}});
    if(!rows.length)return e(Empty,{title:"No regimen comparison",detail:"Assess the patient, then explore a feasible dose and interval grid."});
    var selected=list(props.regimenSelection),multi=props.regimen&&props.regimen.endpointKind==="multi_endpoint",componentResults=list(props.regimen&&props.regimen.componentResults);
    function componentsFor(id){var found=componentResults.filter(function(item){return item.candidateId===id;})[0];return found?list(found.components):[];}
    var headers=multi?["Select","Rank","Regimen","Daily dose","Component attainment","Joint attainment","Expected utility","Hard constraints","Pareto"]:["Select","Rank","Regimen","Daily dose","SS target attainment","SS mean","SS trough–peak","Burden-adjusted score"];
    return e("div",null,
      e("div",{className:"lr-regimen-actions"},
        e("p",null,selected.length?selected.length+" regimen(s) selected. Generate their conditional forecasts for a stacked comparison.":multi?"Hard conditional-draw probability constraints are applied before utility ranking. Pareto candidates are not dominated across the configured endpoints.":"Select one or more candidate rows. Ranking is not an automatic dose recommendation."),
        e(Button,{className:"lr-primary",disabled:!selected.length,onClick:function(){if(props.selectTab)props.selectTab("forecast");emit(props,"predict_regimen",{});}},"Generate "+selected.length+" prediction"+(selected.length===1?"":"s"))),
      e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table lr-regimen-table"},
        e("thead",null,e("tr",null,headers.map(function(x){return e("th",{key:x,title:x==="Joint attainment"?"Probability that all configured endpoints are met in the same conditional ETA draw":x==="Expected utility"?"Weighted mean of component utilities after explicit normalization":x==="Hard constraints"?"Every configured conditional-draw probability constraint must pass before selection":x==="Pareto"?"Not dominated by another eligible regimen across component expected utilities":x==="SS target attainment"?"Probability that the endpoint metric at periodic steady state meets its target":x==="SS mean"?"Conditional median time-weighted concentration over one steady-state dosing interval":x==="SS trough–peak"?"Conditional median trough and peak during one periodic steady-state interval":x==="Burden-adjusted score"?"Lower is preferred: steady-state endpoint loss plus any configured daily-dose burden":undefined},x);}))),
        e("tbody",null,rows.map(function(r,i){var active=selected.indexOf(r.candidate_id)>=0,eligible=r.decision_eligible!==false,components=componentsFor(r.candidate_id);return e("tr",{
          key:r.candidate_id,className:(i===0&&eligible?"lr-best ":"")+(active?"lr-selected ":"")+(!eligible?"lr-infeasible":""),
          tabIndex:eligible?0:-1,role:"checkbox","aria-checked":active,onClick:function(){if(eligible)props.toggleRegimen(r.candidate_id);},
          onKeyDown:function(x){if(eligible&&(x.key==="Enter"||x.key===" ")){x.preventDefault();props.toggleRegimen(r.candidate_id);}}
        },e("td",null,e("span",{className:"lr-radio"+(active?" active":"")},active?"\u2713":"")),e("td",null,value(r.rank,i+1)),
          e("td",null,e("strong",null,r.amount+" every "+r.interval+" h"),e("small",null,r.route,i===0&&eligible?" \u00b7 highest ranked":"")),
          e("td",null,fmt(r.daily_dose,1)),
          multi?e("td",null,e("div",{className:"lr-component-attainment"},components.map(function(component){return e("span",{key:component.component_id,title:component.role+(component.hard_constraint?" hard constraint":"")},component.name+" "+fmt(100*Number(component.attainment_probability),0)+"%");}))):e("td",null,e("div",{className:"lr-prob"},e("i",{style:{width:(100*Number(r.attainment_probability||0))+"%"}}),e("span",null,fmt(100*Number(r.attainment_probability||0),0)+"%"))),
          multi?e("td",null,fmt(100*Number(r.joint_attainment_probability),0)+"%"):e("td",null,fmt(r.steady_state_mean,2)),
          multi?e("td",null,fmt(Number(r.expected_utility),3)):e("td",null,r.profile_type==="steady_state_mean_only"?"Unavailable":fmt(r.steady_state_trough,2)+" – "+fmt(r.steady_state_peak,2)),
          multi?e("td",null,e(Badge,{tone:r.hard_constraints_pass?"covariate":"warning"},r.hard_constraints_pass?"Pass":"Fail")):e("td",null,fmt(r.objective,3)),
          multi?e("td",null,r.pareto_optimal?e(Badge,{tone:"concentration"},"Pareto"):"—"):null);})))));
  }

  function SteadyStateCycle(props) {
    var prediction=props.prediction||{},steady=prediction.steadyState||{},rows=list(prediction.steadyStateForecast),target=prediction.target||null;
    if(steady.profileType==="steady_state_mean_only")return e("div",{className:"lr-callout lr-callout-info"},"This model only predicts average steady-state concentration. A transition or peak-to-trough cycle cannot be inferred from its published structure.");
    if(!rows.length)return null;
    var width=920,height=250,left=64,right=24,top=25,bottom=48,times=rows.map(function(r){return Number(r.time);}),values=[];
    rows.forEach(function(r){values.push(Number(r.lower),Number(r.upper));});if(target)values.push(Number(target.lower),Number(target.upper));values=values.filter(isFinite);
    var minT=Math.min.apply(null,times),maxT=Math.max.apply(null,times),minY=Math.min.apply(null,values),maxY=Math.max.apply(null,values),pad=(maxY-minY)*.12||1;minY=Math.max(0,minY-pad);maxY+=pad;
    function x(v){return left+(Number(v)-minT)/(maxT-minT)*(width-left-right);}function y(v){return top+(maxY-Number(v))/(maxY-minY)*(height-top-bottom);}
    var upper=rows.map(function(r,i){return(i?"L":"M")+x(r.time)+" "+y(r.upper);}).join(" "),lower=rows.map(function(r,i){return(i?"L":"M")+x(r.time)+" "+y(r.lower);}).join(" "),median=rows.map(function(r,i){return(i?"L":"M")+x(r.time)+" "+y(r.median);}).join(" ");
    return e("section",{className:"lr-steady-cycle"},e("h4",null,"Periodic steady-state dosing interval"),e("p",{className:"lr-profile-context"},"The eventual repeating cycle, calculated independently from the transition horizon. Dashed lines show conditional prediction limits."),e("svg",{viewBox:"0 0 "+width+" "+height,role:"img","aria-label":"Periodic steady-state concentration cycle"},target?e("rect",{x:left,y:y(target.upper),width:width-left-right,height:Math.max(1,y(target.lower)-y(target.upper)),className:"lr-target-band"}):null,e("line",{x1:left,y1:height-bottom,x2:width-right,y2:height-bottom,className:"lr-axis"}),e("line",{x1:left,y1:top,x2:left,y2:height-bottom,className:"lr-axis"}),e("path",{d:upper,className:"lr-forecast-limit"}),e("path",{d:lower,className:"lr-forecast-limit"}),e("path",{d:median,className:"lr-forecast-line"}),e("text",{x:width/2,y:height-8,textAnchor:"middle",className:"lr-axis-title"},"Time since dose (hours)"),e("text",{x:16,y:height/2,transform:"rotate(-90 16 "+(height/2)+")",textAnchor:"middle",className:"lr-axis-title"},"Predicted concentration"),e("text",{x:left,y:height-20,className:"lr-chart-label"},"0"),e("text",{x:width-right-20,y:height-20,className:"lr-chart-label"},fmt(maxT,1))));
  }

  function EndpointOutcomeTable(props) {
    var rows=list(props.outcomes);
    if(!rows.length)return null;
    function roleTone(role){return role==="safety"?"warning":role==="primary"?"concentration":"covariate";}
    function roleLabel(role){return role==="primary"?"Primary":role==="safety"?"Safety":"Secondary";}
    function metricLabel(row){return String(value(row.metric,"endpoint metric")).replace(/_/g," ");}
    function intervalLabel(row){var lower=number(row.lower_probability),upper=number(row.upper_probability);return lower!==null&&upper!==null?fmt(100*(upper-lower),0)+"% conditional interval":"Conditional interval";}
    function outcomeUnit(row){return String(value(row.display_unit,"")).trim();}
    return e("section",{className:"lr-endpoint-outcomes"},
      e("div",{className:"lr-endpoint-outcome-heading"},e("h4",null,rows.length>1?"Endpoint outcomes":"Endpoint outcome"),e("p",null,"Numerical conditional-draw outcomes and the targets used for this regimen decision.")),
      e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table lr-endpoint-outcome-table"},
        e("thead",null,e("tr",null,["Endpoint","Predicted outcome","Target","Attainment","Decision rule"].map(function(label){return e("th",{key:label},label);} ))),
        e("tbody",null,rows.map(function(row,index){var suffix=outcomeUnit(row),hard=!!row.hard_constraint,pass=row.constraint_pass!==false,attainment=number(row.attainment_probability),minimum=number(row.minimum_attainment);return e("tr",{key:value(row.component_id,index)},
          e("td",null,e("strong",null,row.name),e("span",{className:"lr-endpoint-role"},e(Badge,{tone:roleTone(row.role)},roleLabel(row.role)),e("small",null,metricLabel(row)))),
          e("td",{className:"lr-endpoint-outcome-value"},e("strong",null,fmt(row.median,3)+(suffix?" "+suffix:"")),e("small",null,intervalLabel(row)+": "+fmt(row.lower,3)+" \u2013 "+fmt(row.upper,3)+(suffix?" "+suffix:""))),
          e("td",{className:"lr-endpoint-target"},row.target),
          e("td",null,e("strong",null,attainment===null?"\u2014":fmt(100*attainment,0)+"%")),
          e("td",null,hard?e("span",{className:"lr-endpoint-rule"},e(Badge,{tone:pass?"covariate":"warning"},pass?"Pass":"Fail"),e("small",null,minimum===null?"Conditional threshold unavailable":"\u2265 "+fmt(100*minimum,0)+"% required")):e("span",{className:"lr-endpoint-rule"},e(Badge,{tone:"neutral"},"No hard gate"),e("small",null,"Contributes to weighted utility"))));})))),
      e("p",{className:"lr-profile-context lr-endpoint-outcome-note"},"Endpoint intervals are quantiles across conditional ETA draws (and residual variability when requested). Population-parameter and model-structure uncertainty are excluded; these are not confidence intervals for a population mean."));
  }

  function ForecastOne(props) {
    var prediction=props.prediction,rows=list(prediction&&prediction.forecast);
    if(!rows.length)return null;
    var probabilities=list(prediction.probabilities),lowerProbability=number(probabilities[0]),upperProbability=number(probabilities[2]),coverage=lowerProbability!==null&&upperProbability!==null?100*(upperProbability-lowerProbability):90,intervalLabel=fmt(coverage,0)+"% conditional prediction interval";
    var width=920,height=360,left=64,right=24,top=28,bottom=55;
    var times=rows.map(function(r){return Number(r.time);}),values=[];
    rows.forEach(function(r){values.push(Number(r.lower),Number(r.upper));});
    var target=prediction.target||null;
    if(target){values.push(Number(target.lower),Number(target.upper));}
    values=values.filter(isFinite);var minT=Math.min.apply(null,times),maxT=Math.max.apply(null,times);if(maxT===minT)maxT=minT+1;
    var minY=Math.min.apply(null,values),maxY=Math.max.apply(null,values),pad=(maxY-minY)*.12||1;minY=Math.max(0,minY-pad);maxY=maxY+pad;
    function x(v){return left+(Number(v)-minT)/(maxT-minT)*(width-left-right);}function y(v){return top+(maxY-Number(v))/(maxY-minY)*(height-top-bottom);}
    var upper=rows.map(function(r,i){return(i?"L":"M")+x(r.time)+" "+y(r.upper);}).join(" "),lower=rows.map(function(r,i){return(i?"L":"M")+x(r.time)+" "+y(r.lower);}).join(" ");
    var median=rows.map(function(r,i){return (i?"L":"M")+x(r.time)+" "+y(r.median);}).join(" ");
    var regimen=prediction.regimen||{},evaluation=prediction.evaluation||{},multi=list(evaluation.components).length>1,interval=Number(regimen.interval),doseTimes=[],steady=prediction.steadyState||{},steadySummary=steady.summary||{},convergence=steady.horizonConvergence||{},meanOnly=steady.profileType==="steady_state_mean_only";
    if(isFinite(interval)&&interval>0){for(var dose=minT;dose<=maxT+1e-8;dose+=interval)doseTimes.push(dose);}
    function steadyMedian(name){return steadySummary[name]&&steadySummary[name].median;}
    var convergenceText=convergence.evaluable?fmt(100*Number(convergence.probability),0)+"% within "+fmt(100*Number(convergence.tolerance),0)+"%":"Not estimable";
    return e("div",{className:"lr-forecast"},
      e("div",{className:"lr-forecast-summary"},
        e("div",null,e("span",null,"Selected regimen"),e("strong",null,fmt(regimen.amount,1)+" every "+fmt(regimen.interval,1)+" h")),
        e("div",null,e("span",null,multi?"Joint target attainment":"Steady-state target attainment"),e("strong",null,fmt(100*Number(multi?regimen.joint_attainment_probability:regimen.attainment_probability),0)+"%")),
        multi?e("div",null,e("span",null,"Expected utility"),e("strong",null,fmt(regimen.expected_utility,3))):null,
        multi?e("div",null,e("span",null,"Hard constraints"),e("strong",{className:regimen.hard_constraints_pass?"":"lr-warn-text"},regimen.hard_constraints_pass?"Pass":"Fail")):null,
        multi?e("div",null,e("span",null,"Pareto status"),e("strong",null,regimen.pareto_optimal?"Pareto-optimal":"Dominated")):null,
        e("div",null,e("span",null,"Steady-state mean"),e("strong",null,fmt(steadyMedian("mean_css"),2))),
        e("div",null,e("span",null,meanOnly?"Within-interval range":"Steady-state trough–peak"),e("strong",null,meanOnly?"Unavailable":fmt(steadyMedian("trough"),2)+" – "+fmt(steadyMedian("peak"),2))),
        e("div",null,e("span",null,"Transition near steady state"),e("strong",{className:convergence.evaluable&&Number(convergence.probability)<.9?"lr-warn-text":""},convergenceText)),
        e("div",null,e("span",null,"Conditional draws"),e("strong",null,rows[0].draws)),
        e("div",null,e("span",null,"Candidate"),e("strong",null,prediction.candidateId))),
      convergence.status?e("div",{className:"lr-callout "+(convergence.evaluable&&Number(convergence.probability)<.9?"lr-callout-warning":"lr-callout-info")},convergence.status+". The steady-state metrics below remain valid even when the transition curve is still settling."):null,
      e(EndpointOutcomeTable,{outcomes:prediction.endpointOutcomes}),
      e("svg",{viewBox:"0 0 "+width+" "+height,role:"img","aria-label":"Future concentration prediction with posterior uncertainty"},
        target?e("rect",{x:left,y:y(target.upper),width:width-left-right,height:Math.max(1,y(target.lower)-y(target.upper)),className:"lr-target-band"}):null,
        e("line",{x1:left,y1:height-bottom,x2:width-right,y2:height-bottom,className:"lr-axis"}),
        e("line",{x1:left,y1:top,x2:left,y2:height-bottom,className:"lr-axis"}),
        e("path",{d:upper,className:"lr-forecast-limit"}),e("path",{d:lower,className:"lr-forecast-limit"}),e("path",{d:median,className:"lr-forecast-line"}),
        doseTimes.map(function(t,i){return e("path",{key:i,d:"M"+(x(t)-4)+" "+(height-bottom+13)+" L"+x(t)+" "+(height-bottom+5)+" L"+(x(t)+4)+" "+(height-bottom+13)+" Z",className:"lr-forecast-dose"});}),
        e("text",{x:left,y:height-18,className:"lr-chart-label"},fmt(minT,1)),e("text",{x:width-right-20,y:height-18,className:"lr-chart-label"},fmt(maxT,1)),
        e("text",{x:left-9,y:y(minY)+3,textAnchor:"end",className:"lr-chart-label"},fmt(minY,2)),e("text",{x:left-9,y:y(maxY)+3,textAnchor:"end",className:"lr-chart-label"},fmt(maxY,2)),
        e("text",{x:width/2,y:height-8,textAnchor:"middle",className:"lr-axis-title"},"Future time (hours)"),
        e("text",{x:16,y:height/2,transform:"rotate(-90 16 "+(height/2)+")",textAnchor:"middle",className:"lr-axis-title"},"Predicted concentration")),
      e("div",{className:"lr-legend"},e("span",null,e("i",{className:"lr-forecast-line-key"}),"Conditional median"),e("span",null,e("i",{className:"lr-forecast-limit-key"}),"Dashed limits: "+intervalLabel),target?e("span",null,e("i",{className:"lr-target-key"}),"Therapeutic range"):null,e("span",null,e("i",{className:"lr-triangle"}),"Future dose")),
      e("p",{className:"lr-profile-context lr-forecast-interval-note"},"The dashed interval is conditional on the selected model and fitted population parameters. It propagates the ETA Laplace approximation and includes residual observation variability only when that option was requested; it is not a full Bayesian posterior predictive interval."),
      e(SteadyStateCycle,{prediction:prediction}),
      e("div",{className:"lr-callout"},"This forecast is conditional on the selected model, available patient evidence, covariate assumptions, adherence, and regimen. It is not a prescription."));
  }
  function Forecast(props) {
    var task=props.task||{},predicting=task.running&&String(task.label).indexOf("Selected-regimen future prediction")>=0;
    if(predicting)return e(TaskProgress,{title:"Generating future predictions",detail:"Propagating conditional ETA uncertainty for the selected regimen(s). Forecasts will appear here when ready.",cancellable:task.cancellable,cancel:function(){emit(props,"cancel_task",{id:task.id});}});
    var predictions=list(props.predictions);
    if(!predictions.length)return e(Empty,{icon:"\u2197",title:"No selected-regimen forecasts",detail:"Open Regimens, select one or more candidates, then generate their future predictions."});
    return e("div",{className:"lr-stacked-forecasts"},predictions.map(function(prediction){return e("section",{key:prediction.id,className:"lr-forecast-card"},e(ForecastOne,{prediction:prediction}));}));
  }

  function Sidebar(props) {
    var search=React.useState(""), patients=list(props.patients).filter(function(p){return (String(p.patient_id)+" "+String(p.label)).toLowerCase().indexOf(search[0].toLowerCase())>=0;});
    var medications=list(props.medications),endpointOptions=list(props.endpoints).filter(function(x){return !props.selectedDrug||x.drugKey===props.selectedDrug;});
    return e("aside",{className:"lr-sidebar"+(props.drawerOpen?" open":"")},e("div",{className:"lr-sidebar-title"},e("strong",null,"Patients"),e(Button,{className:"lr-small lr-primary",icon:"+",onClick:function(){props.open("patient");}},"New")),
      e("div",{className:"lr-search"},e("span",null,"⌕"),e("input",{placeholder:"Search pseudonyms",value:search[0],onChange:function(x){search[1](x.target.value);}})),
      e("div",{className:"lr-patient-list"},patients.length?patients.map(function(p){return e("button",{type:"button",key:p.patient_id,className:props.patient&&props.patient.id===p.patient_id?"active":"",onClick:function(){emit(props,"select_patient",{id:p.patient_id});}},e("span",{className:"lr-avatar"},String(value(p.label,p.patient_id)).slice(0,2).toUpperCase()),e("span",null,e("strong",null,value(p.label,p.patient_id)),e("small",null,p.patient_id)),e(Badge,{tone:"neutral"},p.revision));}):e("div",{className:"lr-mini-empty"},"No patients yet")),
      e("div",{className:"lr-sidebar-section lr-model-select"},e("strong",null,"Medication, endpoint & model"),e("div",{className:"lr-label-action"},e("span",null,"Medication"),e(MedicationAddButton,{disabled:!props.patient,onClick:function(){props.open("medication");}})),e("select",{value:value(props.selectedDrug,""),disabled:!medications.length,onChange:function(x){emit(props,"select_drug",{id:x.target.value});}},e("option",{value:""},medications.length?"Select medication":"Add a medication"),medications.map(function(m){return e("option",{key:m.key,value:m.key},m.drug+(m.endpoint_key?" · objective set":" · endpoint needed"));})),e("label",null,"Therapeutic endpoint",e("select",{value:value(props.selectedEndpoint,""),disabled:!props.selectedDrug,onChange:function(x){emit(props,"select_endpoint",{id:x.target.value});}},e("option",{value:""},"Select endpoint"),endpointOptions.map(function(x){return e("option",{key:x.id,value:x.id},(x.isSet?"Objective: ":"")+x.name+" · v"+value(x.version,"?"));}))),e("div",{className:"lr-label-action lr-model-label-action"},e("span",null,"Population model"),e(Button,{className:"lr-model-info-trigger",disabled:!props.selectedModel||!props.selectedModelInfo,title:"Describe selected model",ariaLabel:"Describe selected model",onClick:function(){props.open("model_info");}},"?")),e("select",{value:value(props.selectedModel,""),disabled:!props.selectedDrug,onChange:function(x){emit(props,"select_model",{id:x.target.value});}},e("option",{value:""},"Select model"),list(props.models).map(function(m){return e("option",{key:m.id,value:m.id},m.name+" · ADVAN"+m.advan);})),e("div",{className:"lr-action-grid"},e(Button,{disabled:!props.patient||!props.selectedDrug||!props.selectedEndpoint||!props.modelDiscoveryAvailable,title:props.modelDiscoveryAvailable?"Match the patient against clinically scoped LibeRary qualifications":props.modelDiscoveryReason,onClick:function(){emit(props,"auto_select_model",{});}},"Find best model"),e(Button,{disabled:!props.patient||!props.selectedDrug,onClick:function(){props.open("model_library");}},"Add/select model"),e(Button,{disabled:!props.selectedDrug,onClick:function(){props.open("endpoint_library");}},"Endpoint library"),e(Button,{disabled:endpointOptions.filter(function(x){return !x.isSet;}).length<2,onClick:function(){props.open("endpoint_set");}},"Combine endpoints"),e(Button,{disabled:!props.selectedEndpoint||!props.endpointEdit,onClick:function(){props.open(props.endpointEdit&&props.endpointEdit.kind==="multi_endpoint"?"endpoint_set_modify":"endpoint_modify");}},"Modify endpoint"))),
      e("div",{className:"lr-sidebar-section"},e("strong",null,"Patient evidence"),e("div",{className:"lr-action-grid"},e(Button,{disabled:!props.patient,icon:"◈",onClick:function(){props.open("dose");}},"Dose"),e(Button,{disabled:!props.patient,icon:"●",onClick:function(){props.open("concentration");}},"TDM"),e(Button,{disabled:!props.patient,icon:"◇",onClick:function(){props.open("covariate");}},"Covariate"),e(Button,{disabled:!props.patient,icon:"⋮",onClick:function(){props.open("state_boundary");}},"State"))),
      e("div",{className:"lr-patient-delete"},e(Button,{className:"lr-danger lr-wide",disabled:!props.patient,onClick:function(){props.open("delete_patient");}},"Delete patient")));
  }
  function ModelSuitability(props) {
    var selection=props.selection;
    if(!selection)return e(Panel,{title:"Model suitability",subtitle:"Not assessed"},e("p",{className:"lr-muted"},"Select an endpoint and ask LibeRator to match the patient against scoped LibeRary qualifications."));
    var candidates=list(selection.candidates),selected=candidates.filter(function(x){return x.id===selection.selectedModelId;})[0];
    var summary=selection.status==="selected"?
      e("div",null,e("strong",{className:"lr-target-name"},value(selected&&selected.name,selection.selectedModelId)),e(Badge,{tone:"covariate"},"Qualified"),e("p",{className:"lr-source"},"Suitability score "+fmt(100*Number(selected&&selected.score),0)+"% · model "+value(selection.selectedModelVersion,"version unavailable"))):
      e("div",null,e("strong",{className:"lr-inline-warning"},selection.status==="multiple_suitable_models"?"Automatic choice withheld":"No suitable qualified model"),e("p",{className:"lr-source"},list(selection.reasons).join("; ")));
    return e(Panel,{title:"Model suitability",subtitle:String(selection.status).replace(/_/g," ")},summary,
      candidates.slice(0,3).map(function(candidate){return e("div",{key:candidate.id,className:"lr-selection-candidate"},e("span",null,candidate.name),e("b",null,candidate.eligible?fmt(100*Number(candidate.score),0)+"%":"Excluded"),list(candidate.blockers).length?e("small",null,list(candidate.blockers).join("; ")):null);}));
  }
  function RightRail(props) {
    var endpoint=list(props.endpoints).filter(function(x){return x.id===props.selectedEndpoint;})[0], current=props.current,readiness=props.readiness||{},ready=!!readiness.assessmentReady,selectedRegimens=list(props.regimenSelection),predictions=list(props.predictions),task=props.task||{},optimising=task.running&&String(task.label).indexOf("Candidate regimen comparison")>=0,predicting=task.running&&String(task.label).indexOf("Selected-regimen future prediction")>=0;
    var changed=readiness.changed||{},steps=[["patient","Patient selected"],["medication","Medication selected"],["endpoint","Endpoint configured"],["model","Population model selected"],["tdm","Measured TDM available"]];
    var missing=steps.filter(function(step){return !readiness[step[0]];})[0];
    return e("aside",{className:"lr-right"+(props.drawerOpen?" open":"")},
      e(Panel,{title:"Assessment readiness",subtitle:ready?"Ready to individualise":missing?"Next: "+missing[1]:"Review inputs"},
        e("div",{className:"lr-readiness"},steps.map(function(step){var altered=changed[step[0]]===true;return e("div",{key:step[0],className:(readiness[step[0]]?"done ":"")+(altered?"changed":"")},e("i",null,readiness[step[0]]?"✓":""),e("span",null,step[1]+(altered?" *":"")));})),
        readiness.anyChanged?e("p",{className:"lr-readiness-note"},"* Changed since the most recent individualisation; rerun the assessment to update the conditional patient estimate."):null,
        !props.modelDiscoveryAvailable?e("p",{className:"lr-inline-warning"},props.modelDiscoveryReason):null),
      e(ModelSuitability,{selection:props.modelSelection}),
      e(Panel,{title:"Current assessment",subtitle:current?current.mode+" conditional MAP fit":"Awaiting TDM update"},
      current?e("div",null,e("div",{className:"lr-metrics"},e("div",null,e("span",null,"Convergence"),e("strong",null,current.convergence===0?"Successful":"Review")),e("div",null,e("span",null,"Fit time"),e("strong",null,fmt(current.diagnostics&&current.diagnostics.elapsed_total_seconds,2)+" s")),e("div",null,e("span",null,"Latent states"),e("strong",null,new Set(list(current.eta).map(function(x){return x.occasion;})).size)),list(current.warnings).length?e("div",null,e("span",null,"Data flags"),e("strong",null,list(current.warnings).length)):null),list(current.warnings).map(function(w,i){return e("div",{key:i,className:"lr-inline-warning"},w);})):e("p",{className:"lr-muted"},missing?"Complete “"+missing[1]+"” before estimating the individual conditional MAP fit.":"Inputs are ready; choose the individualisation model below."),
      e("div",{className:"lr-stack"},e(Button,{className:"lr-primary",disabled:!ready||task.running,title:"One patient-specific ETA vector is estimated across the complete record.",onClick:function(){if(props.selectTab)props.selectTab("posterior");props.open("assessment_static");}},"Estimate stable patient parameters"),e(Button,{disabled:!ready||!readiness.dynamicReady||task.running,title:value(readiness.dynamicReason,"Separate latent parameter states are estimated across declared state boundaries."),onClick:function(){if(props.selectTab)props.selectTab("posterior");props.open("assessment_dynamic");}},"Estimate parameters changing over time"),!readiness.dynamicReady&&ready?e("div",{className:"lr-inline-warning"},value(readiness.dynamicReason,"Add a state boundary and TDM evidence in at least two states.")):null,e("small",{className:"lr-muted"},"Use time-varying parameters only when clinical evidence supports a genuine change; otherwise the stable assessment is easier to interpret."))),
      e(Panel,{title:"Therapeutic objective",subtitle:endpoint?endpoint.status:"No endpoint selected"},endpoint?e("div",null,e("strong",{className:"lr-target-name"},endpoint.name),endpoint.isSet?e("div",{className:"lr-objective-components"},list(endpoint.components).map(function(component){return e("div",{key:component.componentId},e(Badge,{tone:component.role==="safety"?"warning":component.role==="primary"?"concentration":"neutral"},component.role),e("span",null,component.name),e("small",null,"weight "+fmt(component.weight,2)+(component.hardConstraint?" \u00b7 hard \u2265 "+fmt(100*Number(component.minimumAttainment),0)+"%":"")));})):endpoint.lower!==null&&endpoint.lower!==undefined?e("div",{className:"lr-range"},e("span",null,endpoint.lower+" "+endpoint.unit),e("i",null),e("b",null,endpoint.upper+" "+endpoint.unit)):null,e("p",{className:"lr-source"},value(endpoint.source,"No evidence source recorded"))):e("p",{className:"lr-muted"},"Select a versioned endpoint.")),
      e(Panel,{title:"Next step"},predicting?
        e(Button,{className:"lr-primary lr-wide",disabled:true},"Generating future predictions..."):optimising?
        e(Button,{className:"lr-primary lr-wide",disabled:true},"Exploring candidate regimens..."):props.regimen&&selectedRegimens.length&&!predictions.length?
        e(Button,{className:"lr-primary lr-wide",disabled:task.running,onClick:function(){if(props.selectTab)props.selectTab("forecast");emit(props,"predict_regimen",{});}},"Generate "+selectedRegimens.length+" prediction"+(selectedRegimens.length===1?"":"s")):
        e(Button,{className:"lr-primary lr-wide",disabled:!current||task.running,onClick:function(){if(props.selectTab)props.selectTab("regimens");props.open("regimen");}},predictions.length?"Compare other regimens":"Explore candidate regimens"),
        predictions.length?e("p",{className:"lr-muted"},predictions.length+" forecast(s) ready. Review them in the Future prediction tab."):null,
        e("div",{className:"lr-safety-card"},e("strong",null,"Research validation status"),e("p",null,"LibeRator is designed toward clinical use but is not yet clinically validated. Always inspect model qualification, endpoint provenance, missing covariates, fit diagnostics, and uncertainty."))));
  }

  function LibeRatorWorkbench(props) {
    var tab=React.useState("timeline"), modal=React.useState(null), nestedModal=React.useState(null), dark=React.useState(function(){return initialDarkTheme("liberatorTheme");}),sidebarOpen=React.useState(false),railOpen=React.useState(false);
    var regimenSelection=React.useState(list(props.regimen&&props.regimen.selectedCandidates));
    var correctionEvent=React.useState(null);
    var endpointPrompt=React.useRef(0);
    var task=window.LibeRDesign.taskState.use(React,props.inputId,props.task);
    React.useEffect(function(){if(props.regimen&&list(props.regimen.summary).length)tab[1]("regimens");},[props.regimen&&list(props.regimen.summary).length?props.regimen.summary[0].candidate_id:null]);
    React.useEffect(function(){if(list(props.predictions).length)tab[1]("forecast");},[list(props.predictions).map(function(item){return item.id;}).join("|")]);
    React.useEffect(function(){if(task.running&&String(task.label).indexOf("Candidate regimen comparison")>=0)tab[1]("regimens");if(task.running&&String(task.label).indexOf("Selected-regimen future prediction")>=0)tab[1]("forecast");},[task.running,task.label]);
    React.useEffect(function(){var next=Number(props.endpointPrompt)||0;if(next>endpointPrompt.current){endpointPrompt.current=next;modal[1]("endpoint_library");}},[props.endpointPrompt]);
    React.useEffect(function(){storeTheme(dark[0],"liberatorTheme");},[dark[0]]);
    React.useEffect(function(){regimenSelection[1](list(props.regimen&&props.regimen.selectedCandidates));},[list(props.regimen&&props.regimen.selectedCandidates).join("|")]);
    React.useEffect(function(){
      function keydown(event){if(event.key==="Escape"){sidebarOpen[1](false);railOpen[1](false);}}
      document.addEventListener("keydown",keydown);
      return function(){document.removeEventListener("keydown",keydown);};
    },[]);
    function toggle(){dark[1](!dark[0]);}
    function toggleRegimen(id){regimenSelection[1](function(current){var next=current.indexOf(id)>=0?current.filter(function(value){return value!==id;}):current.concat([id]);return next;});emit(props,"select_regimen",{id:id});}
    function closeDrawers(){sidebarOpen[1](false);railOpen[1](false);}
    var tabs=[{id:"timeline",label:"Timeline"},{id:"posterior",label:"Individualisation"},{id:"regimens",label:"Regimens"},{id:"forecast",label:"Future prediction"},{id:"evidence",label:"Evidence ledger"}];
    return e("div",{className:"lr-shell "+(dark[0]?"lr-dark":"lr-light")},
      e("header",{className:"lr-header"},e("div",{className:"lr-brand"},props.icon?e("img",{className:"lr-logo",src:props.icon,alt:""}):e(Logo),e("div",null,e("strong",null,"LibeRator"),e("span",null,"Adaptive Therapeutic Optimisation and Recommendation")),e(Badge,{tone:"research"},"RESEARCH STATUS")),e("div",{className:"lr-header-right"},e("button",{type:"button",className:"lr-drawer-toggle lr-sidebar-toggle","aria-label":"Open patient navigation","aria-expanded":sidebarOpen[0],onClick:function(){sidebarOpen[1](!sidebarOpen[0]);railOpen[1](false);}},"☰"),e("button",{type:"button",className:"lr-drawer-toggle lr-rail-toggle","aria-label":"Open assessment panel","aria-expanded":railOpen[0],onClick:function(){railOpen[1](!railOpen[0]);sidebarOpen[1](false);}},"⌁"),e("span",{className:"lr-header-version"},"v"+value(props.packageVersion,"0.1.0")),props.patient?e("div",{className:"lr-context"},e("span",null,"Active patient"),e("strong",null,value(props.patient.label,props.patient.id))):null,e(ThemeSwitch,{dark:dark[0],onChange:toggle}))),
      e("div",{className:"lr-message lr-message-"+value(props.status&&props.status.level,"info")},e("i",null),e("span",null,task.running?value(task.label,"Background calculation")+" is running":value(props.status&&props.status.text,"Workbench ready")),task.running&&task.cancellable?e(Button,{className:"lr-task-cancel",onClick:function(){emit(props,"cancel_task",{id:task.id});}},"Cancel"):null),
      (sidebarOpen[0]||railOpen[0])?e("button",{type:"button",className:"lr-drawer-backdrop","aria-label":"Close navigation and assessment panels",onClick:closeDrawers}):null,
      e("div",{className:"lr-layout"},e(Sidebar,Object.assign({},props,{open:modal[1],drawerOpen:sidebarOpen[0]})),e("main",{className:"lr-main"},e("div",{className:"lr-tabs"},tabs.map(function(x){return e("button",{type:"button",key:x.id,className:tab[0]===x.id?"active":"",onClick:function(){tab[1](x.id);}},x.label);})),e("div",{className:"lr-canvas"},tab[0]==="timeline"?e(Panel,{title:"Longitudinal response",subtitle:"Doses, samples and latent-state boundaries"},e(Timeline,props)):tab[0]==="posterior"?e(Panel,{title:"Conditional patient states",subtitle:"Population prior updated with this patient's evidence; population parameters and model structure remain fixed"},e(IndividualProfile,props),e(ParameterTable,props)):tab[0]==="regimens"?e(Panel,{title:"Candidate comparison",subtitle:"Select one or more candidates before generating forecasts"},e(RegimenTable,Object.assign({},props,{regimenSelection:regimenSelection[0],toggleRegimen:toggleRegimen,task:task,selectTab:tab[1]}))):tab[0]==="forecast"?e(Panel,{title:"Selected-regimen future predictions",subtitle:"Conditional medians and scoped uncertainty under the proposed dosing schedules"},e(Forecast,Object.assign({},props,{task:task}))):e(Panel,{title:"Immutable evidence ledger",subtitle:"Clinicians can append corrections while the original record and full lineage remain auditable"},e(EvidenceTable,Object.assign({},props,{onCorrect:function(row){correctionEvent[1](row);modal[1]("correct_event");}}))))),e(RightRail,Object.assign({},props,{open:modal[1],drawerOpen:railOpen[0],regimenSelection:regimenSelection[0],task:task,selectTab:tab[1]}))),
      e("footer",{className:"lr-footer"},e("span",null,"LibeRator v"+value(props.packageVersion,"0.1.0")),e("span",null,"Encrypted workspace · C++/automatic differentiation · Human review required")),
      modal[0]==="patient"?e(NewPatientModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="delete_patient"?e(DeletePatientModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="medication"?e(MedicationModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="regimen"?e(RegimenModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="assessment_static"?e(AssessmentModal,{owner:props,mode:"static",onClose:function(){modal[1](null);}}):modal[0]==="assessment_dynamic"?e(AssessmentModal,{owner:props,mode:"dynamic",onClose:function(){modal[1](null);}}):modal[0]==="model_library"?e(ModelLibraryModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="model_info"?e(ModelInfoModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="endpoint_library"?e(EndpointLibraryModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="endpoint_modify"?e(EndpointLibraryModal,{owner:props,editMode:true,onClose:function(){modal[1](null);}}):modal[0]==="endpoint_set"?e(EndpointSetModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="endpoint_set_modify"?e(EndpointSetModal,{owner:props,editMode:true,onClose:function(){modal[1](null);}}):modal[0]==="correct_event"&&correctionEvent[0]?e(EvidenceCorrectionModal,{owner:props,event:correctionEvent[0],onClose:function(){correctionEvent[1](null);modal[1](null);}}):["dose","concentration","covariate","state_boundary"].indexOf(modal[0])>=0?e(EventModal,{owner:props,kind:modal[0],openMedication:function(){nestedModal[1]("medication");},onClose:function(){modal[1](null);}}):null,
      nestedModal[0]==="medication"?e(MedicationModal,{owner:props,nested:true,onClose:function(){nestedModal[1](null);}}):null);
  }
  reactR.reactWidget("liberatorWorkbench", "output", { LibeRatorWorkbench: LibeRatorWorkbench }, {});
}());
