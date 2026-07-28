(function () {
  "use strict";
  var e = React.createElement;
  function list(x) { return Array.isArray(x) ? x : []; }
  function value(x, fallback) { return x === undefined || x === null || x === "" ? fallback : x; }
  function number(x) { var n = Number(x); return isFinite(n) ? n : null; }
  function fmt(x, digits) { var n = Number(x); return isFinite(n) ? n.toFixed(digits === undefined ? 3 : digits).replace(/\.0+$/, "") : "—"; }
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
  function Badge(props) { return e("span", { className: "lr-badge lr-badge-" + value(props.tone, "neutral") }, props.children); }
  function Empty(props) { return e("div", { className: "lr-empty" }, e("span", { className: "lr-empty-icon" }, value(props.icon, "◇")), e("strong", null, props.title), e("p", null, props.detail)); }
  function Panel(props) { return e("section", { className: "lr-panel " + value(props.className, "") },
    e("header", { className: "lr-panel-head" }, e("div", null, e("strong", null, props.title), props.subtitle ? e("span", null, props.subtitle) : null), props.actions || null),
    e("div", { className: "lr-panel-body" }, props.children)); }
  function Logo() { return e("span", { className: "lr-logo lr-logo-fallback", "aria-hidden": "true" }, "L"); }
  function ThemeSwitch(props) { return e("label", { className: "lr-theme-switch", title: "Switch colour theme" },
    e("span", null, props.dark ? "Dark" : "Light"), e("input", { type: "checkbox", checked: props.dark, onChange: props.onChange }), e("i", null)); }

  function Modal(props) { var dialog = useDialogFocus(props.onClose); return e("div", { className: "lr-modal-layer", role: "presentation", onMouseDown: function (x) { if (x.target === x.currentTarget) props.onClose(); } },
    e("section", { ref: dialog, tabIndex: -1, className: "lr-modal", role: "dialog", "aria-modal": "true", "aria-label": props.title },
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
    var time = React.useState(""), name = React.useState(defaults[0]), val = React.useState(""), unit = React.useState(defaults[1]), missing = React.useState(""), route = React.useState("oral"), rate = React.useState("0"), therapeuticClass=React.useState(""),treatmentDrug=React.useState(selectedMedication?selectedMedication.drug:"");
    var medications=list(props.owner.medications),tdmOptions=[];
    medications.forEach(function(m){tdmOptions.push({key:m.key+"|||"+m.drug,drug:m.drug,analyte:m.drug,label:m.drug});String(value(m.monitoring_analytes,"")).split("|").filter(Boolean).forEach(function(analyte){tdmOptions.push({key:m.key+"|||"+analyte,drug:m.drug,analyte:analyte,label:analyte+" ("+m.drug+")"});});});
    React.useEffect(function(){var active=medications.filter(function(item){return item.key===props.owner.selectedDrug;})[0];if(active&&(type==="dose"||type==="concentration")){treatmentDrug[1](active.drug);name[1](active.drug);}},[props.owner.selectedDrug]);
    var title = {dose:"Record dose",concentration:"Record TDM sample",covariate:"Record covariate",state_boundary:"Mark patient-state boundary"}[type];
    return e(Modal, { title: title, subtitle: "New evidence is appended; earlier records are never overwritten", onClose: props.onClose },
      e("div", { className: "lr-form-grid" },
        e(Field, { label: "Timeline time (hours)" }, e("input", { type:"number",step:"any",value:time[0],onChange:function(x){time[1](x.target.value);},autoFocus:true })),
        type === "dose" ? e(Field,{label:"Drug"},e("div",{className:"lr-input-action"},e("select",{value:treatmentDrug[0],onChange:function(x){treatmentDrug[1](x.target.value);name[1](x.target.value);}},e("option",{value:""},"Select added medication"),medications.map(function(m){return e("option",{key:m.key,value:m.drug},m.drug);})),e(Button,{className:"lr-add-medication",title:"Add medication",ariaLabel:"Add medication",onClick:props.openMedication},"+"))) : type === "concentration" ? e(Field,{label:"Drug or monitored analyte"},e("div",{className:"lr-input-action"},e("select",{value:treatmentDrug[0]+"|||"+name[0],onChange:function(x){var chosen=tdmOptions.filter(function(option){return option.key===x.target.value;})[0];if(chosen){treatmentDrug[1](chosen.drug);name[1](chosen.analyte);}}},e("option",{value:"|||"},"Select added medication or analyte"),tdmOptions.map(function(option){return e("option",{key:option.key,value:option.key},option.label);})),e(Button,{className:"lr-add-medication",title:"Add medication",ariaLabel:"Add medication",onClick:props.openMedication},"+"))) : type !== "state_boundary" ? e(Field,{label:"Covariate"},e("input",{value:name[0],onChange:function(x){name[1](x.target.value);}})) : null,
        type !== "state_boundary" ? e(Field, { label: "Value" }, e("input", {type:"number",step:"any",value:val[0],onChange:function(x){val[1](x.target.value);}})) : null,
        type !== "state_boundary" ? e(Field, { label: "Unit" }, e("input", {value:unit[0],onChange:function(x){unit[1](x.target.value);}})) : null,
        type === "dose" ? e(Field, {label:"Route"}, e("select",{value:route[0],onChange:function(x){route[1](x.target.value);}},e("option",null,"oral"),e("option",null,"intravenous"),e("option",null,"other"))) : null,
        type === "dose" ? e(Field, {label:"Rate (amount/hour; 0 = bolus/oral)"}, e("input",{type:"number",step:"any",value:rate[0],onChange:function(x){rate[1](x.target.value);}})) : null,
        type === "dose" ? e(Field, {label:"Therapeutic class (optional)",className:"lr-span-2",help:"Used to suggest an endpoint family when the exact drug is not in the reference presets."}, e("input",{value:therapeuticClass[0],placeholder:"e.g. antiseizure or glycopeptide",onChange:function(x){therapeuticClass[1](x.target.value);}})) : null,
        type !== "dose" && type !== "state_boundary" ? e(Field,{label:"Missing reason",className:"lr-span-2",help:"Required when a scheduled value is unavailable."},e("input",{value:missing[0],onChange:function(x){missing[1](x.target.value);}})) : null),
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",disabled:!time[0] || (type !== "state_boundary" && !name[0]),onClick:function(){emit(props.owner,"add_event",{type:type,time:time[0],name:name[0],treatment_drug:treatmentDrug[0],value:val[0],unit:unit[0],missing_reason:missing[0],route:route[0],rate:rate[0],therapeutic_class:therapeuticClass[0]});props.onClose();}},"Add to timeline")));
  }
  function RegimenModal(props) {
    var amounts=React.useState("100, 200, 300"), intervals=React.useState("12, 24"), horizon=React.useState("168"), nsim=React.useState("100");
    return e(Modal,{title:"Explore candidate regimens",subtitle:"Rank feasible options against the selected endpoint",onClose:props.onClose},
      e("div",{className:"lr-form-grid"},e(Field,{label:"Dose amounts",help:"Comma-separated grid"},e("input",{value:amounts[0],onChange:function(x){amounts[1](x.target.value);}})),e(Field,{label:"Intervals (hours)"},e("input",{value:intervals[0],onChange:function(x){intervals[1](x.target.value);}})),e(Field,{label:"Evaluation horizon (hours)"},e("input",{type:"number",value:horizon[0],onChange:function(x){horizon[1](x.target.value);}})),e(Field,{label:"Posterior draws"},e("input",{type:"number",min:10,value:nsim[0],onChange:function(x){nsim[1](x.target.value);}}))),
      e("div",{className:"lr-callout"},"This comparison reports uncertainty and target attainment; LibeRator currently has Research validation status and does not issue an autonomous prescription."),
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",onClick:function(){emit(props.owner,"optimise",{amounts:amounts[0],intervals:intervals[0],horizon:horizon[0],nsim:nsim[0]});props.onClose();}},"Compare regimens")));
  }
  function EndpointLibraryModal(props) {
    var rows=list(props.owner.endpointTemplates);
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
      edit?e("div",{className:"lr-callout lr-preset-callout"},e("strong",null,"Editing v"+edit.original_version),e("span",null,"Current values are prefilled and remain unchanged in the audit history. Save with the suggested new version or choose another unused version.")):rows.length?e("div",{className:"lr-table-wrap lr-endpoint-library"},e("table",{className:"lr-table"},e("thead",null,e("tr",null,["Select","Endpoint","Metric family","R constructor"].map(function(x){return e("th",{key:x},x);}))),e("tbody",null,rows.map(function(row){var active=selected[0]&&selected[0].id===row.id;return e("tr",{key:row.id,className:"lr-endpoint-template-row"+(active?" lr-selected":""),tabIndex:0,role:"radio","aria-checked":!!active,onClick:function(){choose(row);},onKeyDown:function(x){if(x.key==="Enter"||x.key===" "){x.preventDefault();choose(row);}}},e("td",null,e("span",{className:"lr-radio"+(active?" active":"")},active?"\u2713":"")),e("td",null,e("strong",null,row.name),row.recommended?e(Badge,{tone:"covariate"},"Drug preset"):null,e("small",{className:"lr-source"},row.description)),e("td",null,row.kind),e("td",null,e("code",null,row.constructor)));})))):e("p",{className:"lr-muted"},"No endpoint templates are available."),
      selected[0]?e("section",{className:"lr-endpoint-template-form"},e("h3",null,(edit?"Revise ":"Configure ")+selected[0].name),!edit&&selected[0].preset_label?e("div",{className:"lr-callout lr-preset-callout"},e("strong",null,selected[0].preset_label),e("span",null,"Drug-specific reference values have been prefilled. Review and edit them for the indication, individual target, assay and institutional policy.")):null,e("p",{className:"lr-muted"},"Required values are not universal clinical targets. Review every value and its evidence before registration."),e("div",{className:"lr-form-grid"},fields.map(editor))):e("div",{className:"lr-callout"},"Select an endpoint family to configure its targets, units, evidence source, governance status, and version."),
      e("footer",{className:"lr-modal-actions"},e(Button,{onClick:props.onClose},"Cancel"),e(Button,{className:"lr-primary",disabled:!complete,onClick:function(){emit(props.owner,edit?"revise_endpoint":"create_endpoint",{template_id:selected[0].id,values:values[0],original_key:edit?edit.original_key:""});props.onClose();}},edit?"Save new endpoint version":"Add & select endpoint")));
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
    var rows=list(props.events).slice().sort(function(a,b){return b.time-a.time;});
    if(!rows.length)return e(Empty,{title:"No evidence",detail:"The immutable evidence ledger is empty."});
    return e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table"},e("thead",null,e("tr",null,["Time","Type","Variable","Value","Source"].map(function(x){return e("th",{key:x},x);}))),e("tbody",null,rows.map(function(r){return e("tr",{key:r.id},e("td",null,fmt(r.time,2)),e("td",null,e(Badge,{tone:r.missing?"warning":r.type},r.type)),e("td",null,value(r.name,"—")),e("td",null,r.type==="state_boundary"?"—":r.value===null||r.value===undefined?e("span",{className:"lr-missing"},value(r.missing,"missing")):r.value+" "+value(r.unit,"")),e("td",null,value(r.source,"manual")));}))));
  }
  function ParameterTable(props) {
    var rows=list(props.current&&props.current.eta),parameters=list(props.current&&props.current.individualParameters);
    if(!rows.length&&!parameters.length)return e(Empty,{title:"No individual posterior",detail:"Run a static or dynamic assessment after adding TDM observations."});
    return e("div",{className:"lr-individual-tables"},
      e("section",null,e("h3",null,"Individual random effects (ETAs)"),rows.length?e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table"},e("thead",null,e("tr",null,e("th",null,"State"),e("th",null,"ETA"),e("th",null,"Estimate"),e("th",null,"Posterior SE"))),e("tbody",null,rows.map(function(r,i){return e("tr",{key:i},e("td",null,"Occasion "+value(r.occasion,1)),e("td",null,r.parameter),e("td",null,fmt(r.estimate,4)),e("td",null,fmt(r.standard_error,4)));})))):e("p",{className:"lr-muted"},"No ETA terms in this model.")),
      e("section",null,e("h3",null,"Individualised model parameters"),parameters.length?e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table"},e("thead",null,e("tr",null,e("th",null,"State"),e("th",null,"Parameter"),e("th",null,"Value"),e("th",null,"At time"))),e("tbody",null,parameters.map(function(r,i){return e("tr",{key:i},e("td",null,"Occasion "+value(r.occasion,1)),e("td",null,r.parameter),e("td",null,fmt(r.value,5)),e("td",null,fmt(r.time,2)+" h"));})))):e("p",{className:"lr-muted"},"No structural parameter outputs were exposed by this model.")));
  }
  function IndividualProfile(props) {
    var rows=list(props.current&&props.current.profile).filter(function(r){return number(r.time)!==null&&number(r.ipred)!==null;}).sort(function(a,b){return a.time-b.time;});
    if(!rows.length)return null;
    var width=920,height=330,left=62,right=24,top=25,bottom=48,times=rows.map(function(r){return Number(r.time);}),values=rows.map(function(r){return Number(r.ipred);});
    rows.forEach(function(r){if(number(r.observation)!==null)values.push(Number(r.observation));});
    var minT=Math.min.apply(null,times),maxT=Math.max.apply(null,times);if(maxT===minT)maxT=minT+1;
    var minY=Math.min.apply(null,values),maxY=Math.max.apply(null,values),pad=(maxY-minY)*.12||1;minY=Math.max(0,minY-pad);maxY+=pad;
    function x(v){return left+(Number(v)-minT)/(maxT-minT)*(width-left-right);}function y(v){return top+(maxY-Number(v))/(maxY-minY)*(height-top-bottom);}
    var path=rows.map(function(r,i){return (i?"L":"M")+x(r.time)+" "+y(r.ipred);}).join(" ");
    return e("section",{className:"lr-individual-profile"},e("h3",null,"Individualised PK profile and observations"),e("svg",{viewBox:"0 0 "+width+" "+height,role:"img","aria-label":"Individualised PK prediction and observed concentrations"},e("line",{x1:left,y1:height-bottom,x2:width-right,y2:height-bottom,className:"lr-axis"}),e("line",{x1:left,y1:top,x2:left,y2:height-bottom,className:"lr-axis"}),e("path",{d:path,className:"lr-forecast-line"}),rows.filter(function(r){return number(r.observation)!==null;}).map(function(r,i){return e("circle",{key:i,cx:x(r.time),cy:y(r.observation),r:5,className:"lr-point"});}),e("text",{x:width/2,y:height-8,textAnchor:"middle",className:"lr-axis-title"},"Time (hours)"),e("text",{x:16,y:height/2,transform:"rotate(-90 16 "+(height/2)+")",textAnchor:"middle",className:"lr-axis-title"},"Concentration"),e("text",{x:left,y:height-26,className:"lr-chart-label"},fmt(minT,1)),e("text",{x:width-right-20,y:height-26,className:"lr-chart-label"},fmt(maxT,1))),e("div",{className:"lr-legend"},e("span",null,e("i",{className:"lr-forecast-line-key"}),"Individual prediction"),e("span",null,e("i",{className:"lr-dot"}),"Observation")));
  }
  function RegimenTable(props) {
    var rows=list(props.regimen&&props.regimen.summary);
    if(!rows.length)return e(Empty,{title:"No regimen comparison",detail:"Assess the patient, then explore a feasible dose and interval grid."});
    var selected=list(props.regimen.selectedCandidates);
    return e("div",null,
      e("div",{className:"lr-regimen-actions"},
        e("p",null,selected.length?selected.length+" regimen(s) selected. Generate their posterior forecasts for a stacked comparison.":"Select one or more candidate rows. Ranking is not an automatic dose recommendation."),
        e(Button,{className:"lr-primary",disabled:!selected.length,onClick:function(){emit(props,"predict_regimen",{});}},"Generate "+selected.length+" prediction"+(selected.length===1?"":"s"))),
      e("div",{className:"lr-table-wrap"},e("table",{className:"lr-table lr-regimen-table"},
        e("thead",null,e("tr",null,["Select","Rank","Regimen","Daily dose","Target attainment","Metric","Score"].map(function(x){return e("th",{key:x},x);}))),
        e("tbody",null,rows.map(function(r,i){var active=selected.indexOf(r.candidate_id)>=0,feasible=r.feasible!==false;return e("tr",{
          key:r.candidate_id,className:(i===0?"lr-best ":"")+(active?"lr-selected ":"")+(!feasible?"lr-infeasible":""),
          tabIndex:feasible?0:-1,role:"checkbox","aria-checked":active,onClick:function(){if(feasible)emit(props,"select_regimen",{id:r.candidate_id});},
          onKeyDown:function(x){if(feasible&&(x.key==="Enter"||x.key===" ")){x.preventDefault();emit(props,"select_regimen",{id:r.candidate_id});}}
        },e("td",null,e("span",{className:"lr-radio"+(active?" active":"")},active?"\u2713":"")),e("td",null,i+1),
          e("td",null,e("strong",null,r.amount+" every "+r.interval+" h"),e("small",null,r.route,i===0?" \u00b7 highest ranked":"")),
          e("td",null,fmt(r.daily_dose,1)),e("td",null,e("div",{className:"lr-prob"},e("i",{style:{width:(100*Number(r.attainment_probability||0))+"%"}}),e("span",null,fmt(100*Number(r.attainment_probability||0),0)+"%"))),e("td",null,fmt(r.median_metric,2)),e("td",null,fmt(r.objective,3)));})))));
  }

  function ForecastOne(props) {
    var prediction=props.prediction,rows=list(prediction&&prediction.forecast);
    if(!rows.length)return null;
    var width=920,height=360,left=64,right=24,top=28,bottom=55;
    var times=rows.map(function(r){return Number(r.time);}),values=[];
    rows.forEach(function(r){values.push(Number(r.lower),Number(r.upper));});
    var target=prediction.target||null;
    if(target){values.push(Number(target.lower),Number(target.upper));}
    values=values.filter(isFinite);var minT=Math.min.apply(null,times),maxT=Math.max.apply(null,times);if(maxT===minT)maxT=minT+1;
    var minY=Math.min.apply(null,values),maxY=Math.max.apply(null,values),pad=(maxY-minY)*.12||1;minY=Math.max(0,minY-pad);maxY=maxY+pad;
    function x(v){return left+(Number(v)-minT)/(maxT-minT)*(width-left-right);}function y(v){return top+(maxY-Number(v))/(maxY-minY)*(height-top-bottom);}
    var upper=rows.map(function(r){return x(r.time)+","+y(r.upper);}).join(" "),lower=rows.slice().reverse().map(function(r){return x(r.time)+","+y(r.lower);}).join(" ");
    var median=rows.map(function(r,i){return (i?"L":"M")+x(r.time)+" "+y(r.median);}).join(" ");
    var regimen=prediction.regimen||{},interval=Number(regimen.interval),doseTimes=[];
    if(isFinite(interval)&&interval>0){for(var dose=minT;dose<=maxT+1e-8;dose+=interval)doseTimes.push(dose);}
    return e("div",{className:"lr-forecast"},
      e("div",{className:"lr-forecast-summary"},
        e("div",null,e("span",null,"Selected regimen"),e("strong",null,fmt(regimen.amount,1)+" every "+fmt(regimen.interval,1)+" h")),
        e("div",null,e("span",null,"Target attainment"),e("strong",null,fmt(100*Number(regimen.attainment_probability),0)+"%")),
        e("div",null,e("span",null,"Posterior draws"),e("strong",null,rows[0].draws)),
        e("div",null,e("span",null,"Candidate"),e("strong",null,prediction.candidateId))),
      e("svg",{viewBox:"0 0 "+width+" "+height,role:"img","aria-label":"Future concentration prediction with posterior uncertainty"},
        target?e("rect",{x:left,y:y(target.upper),width:width-left-right,height:Math.max(1,y(target.lower)-y(target.upper)),className:"lr-target-band"}):null,
        e("line",{x1:left,y1:height-bottom,x2:width-right,y2:height-bottom,className:"lr-axis"}),
        e("line",{x1:left,y1:top,x2:left,y2:height-bottom,className:"lr-axis"}),
        e("polygon",{points:upper+" "+lower,className:"lr-forecast-band"}),e("path",{d:median,className:"lr-forecast-line"}),
        doseTimes.map(function(t,i){return e("path",{key:i,d:"M"+(x(t)-4)+" "+(height-bottom+13)+" L"+x(t)+" "+(height-bottom+5)+" L"+(x(t)+4)+" "+(height-bottom+13)+" Z",className:"lr-forecast-dose"});}),
        e("text",{x:left,y:height-18,className:"lr-chart-label"},fmt(minT,1)),e("text",{x:width-right-20,y:height-18,className:"lr-chart-label"},fmt(maxT,1)),
        e("text",{x:left-9,y:y(minY)+3,textAnchor:"end",className:"lr-chart-label"},fmt(minY,2)),e("text",{x:left-9,y:y(maxY)+3,textAnchor:"end",className:"lr-chart-label"},fmt(maxY,2)),
        e("text",{x:width/2,y:height-8,textAnchor:"middle",className:"lr-axis-title"},"Future time (hours)"),
        e("text",{x:16,y:height/2,transform:"rotate(-90 16 "+(height/2)+")",textAnchor:"middle",className:"lr-axis-title"},"Predicted concentration")),
      e("div",{className:"lr-legend"},e("span",null,e("i",{className:"lr-forecast-line-key"}),"Posterior median"),e("span",null,e("i",{className:"lr-forecast-band-key"}),"90% interval"),target?e("span",null,e("i",{className:"lr-target-key"}),"Therapeutic range"):null,e("span",null,e("i",{className:"lr-triangle"}),"Future dose")),
      e("div",{className:"lr-callout"},"This forecast is conditional on the selected model, available patient evidence, covariate assumptions, adherence, and regimen. It is not a prescription."));
  }
  function Forecast(props) {
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
      e("div",{className:"lr-patient-delete"},e(Button,{className:"lr-danger lr-wide",disabled:!props.patient,onClick:function(){props.open("delete_patient");}},"Delete patient")),
      e("div",{className:"lr-sidebar-section"},e("strong",null,"Evidence"),e("div",{className:"lr-action-grid"},e(Button,{disabled:!props.patient,icon:"◈",onClick:function(){props.open("dose");}},"Dose"),e(Button,{disabled:!props.patient,icon:"●",onClick:function(){props.open("concentration");}},"TDM"),e(Button,{disabled:!props.patient,icon:"◇",onClick:function(){props.open("covariate");}},"Covariate"),e(Button,{disabled:!props.patient,icon:"⋮",onClick:function(){props.open("state_boundary");}},"State"))),
      e("div",{className:"lr-sidebar-section lr-model-select"},e("strong",null,"Medication, model & endpoint"),e("div",{className:"lr-label-action"},e("span",null,"Medication"),e(Button,{className:"lr-add-medication",disabled:!props.patient,title:"Add medication",ariaLabel:"Add medication",onClick:function(){props.open("medication");}},"+")),e("select",{value:value(props.selectedDrug,""),disabled:!medications.length,onChange:function(x){emit(props,"select_drug",{id:x.target.value});}},e("option",{value:""},medications.length?"Select medication":"Add a medication"),medications.map(function(m){return e("option",{key:m.key,value:m.key},m.drug+(m.endpoint_key?" · endpoint set":" · endpoint needed"));})),e("label",null,"Population model",e("select",{value:value(props.selectedModel,""),disabled:!props.selectedDrug,onChange:function(x){emit(props,"select_model",{id:x.target.value});}},e("option",{value:""},"Select model"),list(props.models).map(function(m){return e("option",{key:m.id,value:m.id},m.name+" · ADVAN"+m.advan);}))),e("label",null,"Therapeutic endpoint",e("select",{value:value(props.selectedEndpoint,""),disabled:!props.selectedDrug,onChange:function(x){emit(props,"select_endpoint",{id:x.target.value});}},e("option",{value:""},"Select endpoint"),endpointOptions.map(function(x){return e("option",{key:x.id,value:x.id},x.name+" · v"+value(x.version,"?"));}))),e("div",{className:"lr-action-grid"},e(Button,{disabled:!props.patient||!props.selectedDrug||!props.selectedEndpoint,onClick:function(){emit(props,"auto_select_model",{});}},"Find best model"),e(Button,{disabled:!props.selectedDrug,onClick:function(){props.open("endpoint_library");}},"Endpoint library"),e(Button,{disabled:!props.selectedEndpoint||!props.endpointEdit,onClick:function(){props.open("endpoint_modify");}},"Modify endpoint"))));
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
    var endpoint=list(props.endpoints).filter(function(x){return x.id===props.selectedEndpoint;})[0], current=props.current, ready=props.patient&&props.selectedModel&&props.selectedEndpoint,selectedRegimens=list(props.regimen&&props.regimen.selectedCandidates),predictions=list(props.predictions);
    return e("aside",{className:"lr-right"+(props.drawerOpen?" open":"")},
      e(ModelSuitability,{selection:props.modelSelection}),
      e(Panel,{title:"Current assessment",subtitle:current?current.mode+" posterior":"Awaiting TDM update"},
      current?e("div",null,e("div",{className:"lr-metrics"},e("div",null,e("span",null,"Convergence"),e("strong",null,current.convergence===0?"Successful":"Review")),e("div",null,e("span",null,"Fit time"),e("strong",null,fmt(current.diagnostics&&current.diagnostics.elapsed_total_seconds,2)+" s")),e("div",null,e("span",null,"Latent states"),e("strong",null,new Set(list(current.eta).map(function(x){return x.occasion;})).size)),list(current.warnings).length?e("div",null,e("span",null,"Data flags"),e("strong",null,list(current.warnings).length)):null),list(current.warnings).map(function(w,i){return e("div",{key:i,className:"lr-inline-warning"},w);})):e("p",{className:"lr-muted"},"Add at least one measured concentration, then estimate the patient's posterior PK state."),
      e("div",{className:"lr-stack"},e(Button,{className:"lr-primary",disabled:!ready,onClick:function(){emit(props,"assess",{mode:"static"});}},"Update static posterior"),e(Button,{disabled:!ready,onClick:function(){emit(props,"assess",{mode:"dynamic",process_scale:.1});}},"Update time-varying posterior"))),
      e(Panel,{title:"Therapeutic objective",subtitle:endpoint?endpoint.status:"No endpoint selected"},endpoint?e("div",null,e("strong",{className:"lr-target-name"},endpoint.name),endpoint.lower!==null&&endpoint.lower!==undefined?e("div",{className:"lr-range"},e("span",null,endpoint.lower+" "+endpoint.unit),e("i",null),e("b",null,endpoint.upper+" "+endpoint.unit)):null,e("p",{className:"lr-source"},value(endpoint.source,"No evidence source recorded"))):e("p",{className:"lr-muted"},"Select a versioned endpoint.")),
      e(Panel,{title:"Next step"},props.regimen&&selectedRegimens.length&&!predictions.length?
        e(Button,{className:"lr-primary lr-wide",onClick:function(){emit(props,"predict_regimen",{});}},"Generate "+selectedRegimens.length+" prediction"+(selectedRegimens.length===1?"":"s")):
        e(Button,{className:"lr-primary lr-wide",disabled:!current,onClick:function(){props.open("regimen");}},predictions.length?"Compare other regimens":"Explore candidate regimens"),
        predictions.length?e("p",{className:"lr-muted"},predictions.length+" forecast(s) ready. Review them in the Future prediction tab."):null,
        e("div",{className:"lr-safety-card"},e("strong",null,"Research validation status"),e("p",null,"LibeRator is designed toward clinical use but is not yet clinically validated. Always inspect model qualification, endpoint provenance, missing covariates, fit diagnostics, and uncertainty."))));
  }

  function LibeRatorWorkbench(props) {
    var tab=React.useState("timeline"), modal=React.useState(null), nestedModal=React.useState(null), dark=React.useState(function(){return initialDarkTheme("liberatorTheme");}),sidebarOpen=React.useState(false),railOpen=React.useState(false);
    var endpointPrompt=React.useRef(0);
    var task=window.LibeRDesign.taskState.use(React,props.inputId,props.task);
    React.useEffect(function(){if(props.regimen&&list(props.regimen.summary).length)tab[1]("regimens");},[props.regimen&&list(props.regimen.summary).length?props.regimen.summary[0].candidate_id:null]);
    React.useEffect(function(){if(list(props.predictions).length)tab[1]("forecast");},[list(props.predictions).map(function(item){return item.id;}).join("|")]);
    React.useEffect(function(){var next=Number(props.endpointPrompt)||0;if(next>endpointPrompt.current){endpointPrompt.current=next;modal[1]("endpoint_library");}},[props.endpointPrompt]);
    React.useEffect(function(){storeTheme(dark[0],"liberatorTheme");},[dark[0]]);
    React.useEffect(function(){
      function keydown(event){if(event.key==="Escape"){sidebarOpen[1](false);railOpen[1](false);}}
      document.addEventListener("keydown",keydown);
      return function(){document.removeEventListener("keydown",keydown);};
    },[]);
    function toggle(){dark[1](!dark[0]);}
    function closeDrawers(){sidebarOpen[1](false);railOpen[1](false);}
    var tabs=[{id:"timeline",label:"Timeline"},{id:"posterior",label:"Individualisation"},{id:"regimens",label:"Regimens"},{id:"forecast",label:"Future prediction"},{id:"evidence",label:"Evidence ledger"}];
    return e("div",{className:"lr-shell "+(dark[0]?"lr-dark":"lr-light")},
      e("header",{className:"lr-header"},e("div",{className:"lr-brand"},props.icon?e("img",{className:"lr-logo",src:props.icon,alt:""}):e(Logo),e("div",null,e("strong",null,"LibeRator"),e("span",null,"Adaptive Therapeutic Optimisation and Recommendation")),e(Badge,{tone:"research"},"RESEARCH STATUS")),e("div",{className:"lr-header-right"},e("button",{type:"button",className:"lr-drawer-toggle lr-sidebar-toggle","aria-label":"Open patient navigation","aria-expanded":sidebarOpen[0],onClick:function(){sidebarOpen[1](!sidebarOpen[0]);railOpen[1](false);}},"☰"),e("button",{type:"button",className:"lr-drawer-toggle lr-rail-toggle","aria-label":"Open assessment panel","aria-expanded":railOpen[0],onClick:function(){railOpen[1](!railOpen[0]);sidebarOpen[1](false);}},"⌁"),props.patient?e("div",{className:"lr-context"},e("span",null,"Active patient"),e("strong",null,value(props.patient.label,props.patient.id))):null,e(ThemeSwitch,{dark:dark[0],onChange:toggle}))),
      e("div",{className:"lr-message lr-message-"+value(props.status&&props.status.level,"info")},e("i",null),e("span",null,task.running?value(task.label,"Background calculation")+" is running":value(props.status&&props.status.text,"Workbench ready")),task.running&&task.cancellable?e(Button,{className:"lr-task-cancel",onClick:function(){emit(props,"cancel_task",{id:task.id});}},"Cancel"):null),
      (sidebarOpen[0]||railOpen[0])?e("button",{type:"button",className:"lr-drawer-backdrop","aria-label":"Close navigation and assessment panels",onClick:closeDrawers}):null,
      e("div",{className:"lr-layout"},e(Sidebar,Object.assign({},props,{open:modal[1],drawerOpen:sidebarOpen[0]})),e("main",{className:"lr-main"},e("div",{className:"lr-tabs"},tabs.map(function(x){return e("button",{type:"button",key:x.id,className:tab[0]===x.id?"active":"",onClick:function(){tab[1](x.id);}},x.label);})),e("div",{className:"lr-canvas"},tab[0]==="timeline"?e(Panel,{title:"Longitudinal response",subtitle:"Doses, samples and latent-state boundaries"},e(Timeline,props)):tab[0]==="posterior"?e(Panel,{title:"Posterior patient states",subtitle:"Population prior updated with this patient's evidence"},e(IndividualProfile,props),e(ParameterTable,props)):tab[0]==="regimens"?e(Panel,{title:"Candidate comparison",subtitle:"Select one or more candidates before generating forecasts"},e(RegimenTable,props)):tab[0]==="forecast"?e(Panel,{title:"Selected-regimen future predictions",subtitle:"Posterior medians and uncertainty propagated under the proposed dosing schedules"},e(Forecast,props)):e(Panel,{title:"Immutable evidence ledger",subtitle:"Corrections append new evidence rather than replacing history"},e(EvidenceTable,props)))),e(RightRail,Object.assign({},props,{open:modal[1],drawerOpen:railOpen[0]}))),
      e("footer",{className:"lr-footer"},e("span",null,"LibeRator v"+value(props.packageVersion,"0.1.0")),e("span",null,"Encrypted workspace · C++/automatic differentiation · Human review required")),
      modal[0]==="patient"?e(NewPatientModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="delete_patient"?e(DeletePatientModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="medication"?e(MedicationModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="regimen"?e(RegimenModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="endpoint_library"?e(EndpointLibraryModal,{owner:props,onClose:function(){modal[1](null);}}):modal[0]==="endpoint_modify"?e(EndpointLibraryModal,{owner:props,editMode:true,onClose:function(){modal[1](null);}}):["dose","concentration","covariate","state_boundary"].indexOf(modal[0])>=0?e(EventModal,{owner:props,kind:modal[0],openMedication:function(){nestedModal[1]("medication");},onClose:function(){modal[1](null);}}):null,
      nestedModal[0]==="medication"?e(MedicationModal,{owner:props,nested:true,onClose:function(){nestedModal[1](null);}}):null);
  }
  reactR.reactWidget("liberatorWorkbench", "output", { LibeRatorWorkbench: LibeRatorWorkbench }, {});
}());
