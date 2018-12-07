/*
 *  ciao-playground-ui.js
 *
 *  Common Playground user interface for Ciao (support for buffers,
 *  menus, modal dialogs, setting from menu_generator.pl, etc.)
 *
 *  Copyright (C) 2017 Jose F. Morales
 */

/* TODO: Improve documentation */

/* requires: CodeMirror */
/* requires: split.min.js */

var Playground = (function () {
  function Playground(div_id) {
    this.div_id = div_id;
    this.event = {}; /* events handler */
    this.buf_editor = {}; /* Buffers in the editor */
    this.submit_opts = null; /* TODO: do not use in RPC code directly */
    /* this.submit_data = null; */ /* TODO: computed from opts */
    this.submit_cmd = null;
  }
  Playground.prototype.hide = function() {
    set_visible(this.div_id, false);
  };
  Playground.prototype.show = function() {
    set_visible(this.div_id, true);
    Object.keys(this.buf_editor).forEach((b) => {
      this.buf_editor[b].refresh();
    });
  };
  Playground.prototype.setup = function(title) {
    var w = document.getElementById(this.div_id);

    clean_div(w);

    /* In-progress indicator (an UNICODE hourglass) */
    var dr = document.createElement('div');
    dr.className = "in-progress";
    dr.id = "in-progress";
    dr.style.display = "none";
    dr.appendChild(document.createTextNode("⌛"));
    w.appendChild(dr);

    /* Toolbar */
    var dm = document.createElement('div');
    dm.className = "frame_menu";
    dm.id = "frame_menu";
    w.appendChild(dm);

    /* Toolbar title */
    var dt = document.createElement('span');
    dt.className = "frame_title";
    dt.id = "frame_title";
    dm.appendChild(dt);

    /* Toolbar buttons */
    var dc = document.createElement('div');
    dc.className = "frame_buttons";
    dc.id = "frame_buttons";
    dm.appendChild(dc);

    /* Buffers */
    var db = document.createElement('div');
    db.className = "frame_buffers";
    db.id = "frame_buffers";
    w.appendChild(db);

    /* Prepare modal windows */
    /* (initially hidden) */
    modal_alloc(w);

    /* Set title */
    clean_div(dt);
    add_html(dt, title);

    /* Buffers */
    this._create_buffers();
  }

  Playground.prototype._create_buffers = function() {
    var b1 = document.createElement('div');
    b1.id = "buffers";
    b1.className = "split";
    b1.appendChild(create_buf('in-buffer', "Input", true));
    b1.appendChild(create_buf('out-buffer', "Output", true));
    var b2 = create_buf('console', "Console", false);
    b2.id = "console";

    var w = document.getElementById('frame_buffers');
    clean_div(w);
    w.appendChild(b1);
    w.appendChild(b2);

    Split(['#buffers', '#console'], {
      direction: 'vertical',
      sizes: [75, 25],
      gutterSize: 2,
      cursor: 'col-resize'
    });
    Split(['#in-buffer', '#out-buffer'], {
      gutterSize: 2,
      cursor: 'col-resize'
    });

    function new_cm(id, lineNumbers) {
      return CodeMirror.fromTextArea(document.getElementById(id), {
	lineNumbers: lineNumbers,
	mode: "text/plain",
	keyMap: "emacs"
      });
    }

    this.buf_editor['in'] = new_cm("in-buffer-text", true);
    this.buf_editor['out'] = new_cm("out-buffer-text", true);
    this.buf_editor['console'] = new_cm("console-text", false);
  };

  /* Create a buffer */
  function create_buf(id, title, horiz) {
    var d = document.createElement('div');
    d.id = id;
    if (horiz) {
      d.className = "split split-horizontal content";
    } else {
      d.className = "split content";
    }
    var dt = document.createElement('div');
    dt.className = "buffer_title";
    dt.appendChild(document.createTextNode(title));
    d.appendChild(dt);
    var contents = document.createElement('div');
    contents.className = "buffer_contents";
    contents.id = id + '-contents'; /* for set_html_view */
    var ta = document.createElement('textarea');
    ta.className = 'CodeMirror';
    ta.id = id + '-text';
    ta.name = id + '-text';
    ta.style = "width:100%; height:100%;";
    ta.wrap = "soft";
    contents.appendChild(ta);
    d.appendChild(contents);
    return d;
  }

  /* Show a status message */
  Playground.prototype.show_status = function(msg) {
    if (msg == null) {
      this.buf_editor['console'].setValue("");
    } else {
      this.buf_editor['console'].setValue(msg);
    }
  };

  /* Enable/disable in-progress indicator */
  Playground.prototype.show_in_progress = function(status) {
    set_visible('in-progress', status);
  };

  /* Modal windows */

  function modal_elm() {
    return document.getElementById('modal-content');
  }

  function modal_show() {
    set_visible('modal-screen', true);
    set_visible('modal-content', true);
  }

  function modal_close() {
    set_visible('modal-screen', false);
    set_visible('modal-content', false);
  }

  function modal_alloc(w) {
    var dp = document.createElement('div');
    dp.id = "modal-screen";
    dp.style.display = "none";
    var dpc = document.createElement('div');
    dpc.id = "modal-content";
    dpc.style.display = "none";
    dp.appendChild(dpc);
    w.appendChild(dp);
  }

  /* Auxiliary */
  function clean_div(elm) {
    while (elm.hasChildNodes()) {
      elm.removeChild(elm.firstChild);
    }
  }

  /* Auxiliary */
  function elements_from_string(str) {
    var t = document.createElement('template');
    t.innerHTML = str;
    return t.content; /* a DocumentFragment */
  }

  /* Auxiliary */
  function set_visible(div_id, visible) {
    var elm = document.getElementById(div_id);
    var d = visible ? 'block' : 'none';
    if (elm.style.display !== d) {
      elm.style.display = d;
    }
  }

  /* Fill UI elements */
  Playground.prototype._fill_ui = function(elm, defs) {
    defs.forEach((d) => {
      switch(d.tag) {
      case "html": add_html(elm, d.value); break;
      case "itemlist": this._fill_itemlist(elm, d.value); break;
      case "toolbar": this._fill_toolbar(elm, d.value); break;
      case "menu": this._fill_menu(elm, d.value); break;
      default: throw "unknown tag " + d.tag;
      }
    });
  };

  /* Fill menu settings (flags) */
  Playground.prototype._fill_menu = function(elm, menudef) {
    var dm = document.createElement('div');
    elm.appendChild(dm);
    menu_items_load(menudef);
    menu_items_init();
    menu_items_create_elem(dm);
    menu_items_fill_elem();
  };

  /* NOTE: combos are not generated at this point, so we cannot access them. */
  function menu_items_init() {
    menu_items_update_def(); /* set default values */
    menu_items_exec_guards(); /* recompute guards */
  }
  function menu_items_load(menudef) {
    menu_items_for_flag = {};
    menus = [];
    menudef.forEach((m) => { add_menu(m); });
  }
  function menu_items_create_elem(elm) {
    menus.forEach((m) => { m.create_elem(elm) });
  }

  /* Fill a toolbar */
  Playground.prototype._fill_toolbar = function(elm, button_list) {
    var d = document.createElement('div');
    elm.appendChild(d);
    button_list.forEach((b) => { this._add_button(d, b); });
  };

  /* Add a toolbar button */
  Playground.prototype._add_button = function(elm, opts) {
    if (opts['upload'] === true) {
      add_upload_button(elm);
    }
    d = document.createElement('button');
    d.className = opts.className ? opts.className : 'barbutton';
    d.type = 'submit';
    d.name = 'action';
    d.value = opts.value;
    var self = this;
    d.onclick = function () { self.set_submit(this, opts); return true; };
    d.appendChild(document.createTextNode(opts.text));
    elm.appendChild(d);
  };

  function add_upload_button(elm) {
    var d = document.createElement('input');
    d.type = 'file';
    d.name = 'file';
    elm.appendChild(d);
  }

  /* Record button command on click */
  Playground.prototype.set_submit = function(btn, opts) {
    this.submit_cmd = btn.value;
    this.submit_opts = opts;
  }

  Playground.prototype._setup_form = function(w, defs) {
    /* Create modal (if needed), form and hooks on submit */
    var mdl = modal_elm();
    var is_modal = (mdl !== undefined) && (w === mdl);
    var f = document.createElement('form');
    f.action = '#';
    f.onsubmit = () => {
      if (is_modal) modal_close(); /* TODO: include in submit_opts instead? */
      var data;
      data = this.form_to_cmd_data(f, this.submit_opts);
      if (this.submit_opts['cmd_args'] !== undefined) {
	/* TODO: partial apply? */
	data['args'] = this.submit_opts['cmd_args'];
      }
      this.event['submit_cmd'](this.submit_cmd, this.submit_opts, data, 
			       function(res) {});
      return false;
    };
    /* Fill UI elements */
    this._fill_ui(f, defs);
    /* Attach and show if needed */
    clean_div(w);
    w.appendChild(f);
    if (is_modal) modal_show();
  };

  /* Extract command data (key and values) from a form input (only for
   * non disabled elements) */
  /* TODO: do not use the form, except for uploading files! (try avoid
   *   methods on FormData for Safari compatibility*/
  /* TODO: multiple selection is not handle properly --- e.g., for checkboxes */
  /* TODO: add explicit support for checkboxes? as a list of values */
  Playground.prototype.form_to_cmd_data = function(form, submit_opts) {
    var formData = new FormData(form);
    var data;
    data = {};
    /* Extract menu values */
    var menu_values = {};
    for (var key in menu_items_for_flag) {
      var m = find_menu_item(key);
      if (m !== undefined) { /* Menu item is active! */
	// console.log("m:" + m.menu + " l:" + m.level + " k:" + key + " v: " + m.get_flag_value());
	menu_values[key] = [m.menu, m.level, m.get_flag_value()]; /* TODO: not nice */
      }
    }
    data['menu_values'] = menu_values;
    /* Add files and input */
    if (formData.get === undefined) {
      console.log("bug: uploading files in Safari is not currently supported");
    } else {
      var file = formData.get('file');
      if (file !== null) {
	data['file'] = file;
      }
    }
    if (submit_opts['use_input'] === true) {
      data['blob1'] = this.buf_editor['in'].getValue();
    }
    return data;
  };

  function add_menu(opts) {
    var flag = opts.flag;
    var m = new MenuItem(opts.menu, opts.level, flag, opts.title, opts.help, opts.options, opts.def_opt, opts.guard);
    if (menu_items_for_flag[flag] === undefined) {
      menu_items_for_flag[flag] = [];
    }
    menu_items_for_flag[flag].push(m);
    menus.push(m);
  }

  /* Menus */
  var menu_items_for_flag = {};
  var menus = [];

  /* Get the active (in_use) menu item for the given flag. Return
   * undefined otherwise */
  function find_menu_item(flag) {
    var ms = menu_items_for_flag[flag];
    var m;
    if (ms === undefined) return undefined;
    for (var i = 0; i < ms.length; i++) {
      m = ms[i];
      if (m === undefined) return undefined; /* TODO: useless sentence? */
      if (m.in_use == false) continue; /* Try next one */
      return m;
    }
    return undefined; /* None was active! */
  }

  /* Get the value from an active (in_use) menu item. Return undefined
   * otherwise */
  function menu_value(flag) {
    var m = find_menu_item(flag);
    if (m === undefined) return undefined; /* TODO: useless sentence? */
    return m.get_flag_value();
  }

  /* TODO: rename 'options' by domain, type, etc. */
  function MenuItem(menu, level, flag, title, help, options, def_opt, guard) {
    this.elem = null;
    this.input_elem = null;
    this.help_elem = null;
    this.menu = menu;
    this.level = level;
    this.flag = flag;
    this.title = title;
    this.help = help;
    this.options = options;
    this.def_opt = def_opt;
    this.guard = guard; /* TODO: document (array of arrays of arrays) */
    this.selected_value = null;
    this.in_use = true;
    /* TODO: support 'int', 'nnegint', 'atom', 'atm', see auto_interface:hook_menu_flag_values */
    this.is_combo = Array.isArray(options);
  }

  MenuItem.prototype.create_elem = function(elm) {
    var d = document.createElement('div');
    d.className = 'menu_item';
    this.elem = d;
    /* Title and flag name */
    var dt = document.createElement('div');
    dt.className = 'menu_title';
    dt.appendChild(document.createTextNode(this.title));
    var df = document.createElement('span');
    df.className = 'menu_flag';
    df.appendChild(document.createTextNode("["+this.flag+"]"));
    dt.appendChild(document.createTextNode("\u00A0\u00A0\u00A0"));
    dt.appendChild(df);
    d.appendChild(dt);
    /* Input for flag */
    /* Note: there may exists several elements with the same 'name'
     * property. Only the values of those activated will be considered. */
    var di;
    if (this.is_combo) {
      di = document.createElement('select');
      di.name = this.flag;
      di.className = 'select';
      var self = this;
      di.onchange = function() {
        self.update_flag_with_index(this.selectedIndex);
        menu_items_recompute_guards();
      };
    } else {
      di = document.createElement('input');
      di.name = this.flag;
      di.className = 'text';
      var self = this;
      di.onchange = function() {
        self.update_flag_with_value(this.value);
        menu_items_recompute_guards();
      };
    }
    this.input_elem = di;
    d.appendChild(di);
    /* Verbose description */
    var dh = document.createElement('div');
    this.help_elem = dh;
    dh.appendChild(document.createTextNode(this.help));
    dh.className = 'menu_desc';
    d.appendChild(dh);
    /* */
    elm.appendChild(d);
  };

  /* Re-execute guard */
  /* TODO: fixpo? */
  MenuItem.prototype.exec_guard = function() {
    this.in_use = this.guard.some((and) => {
      return and.every((lit) => {
	/* lit = [Cond, FlagName, Value] */
	var fv;
	fv = menu_value(lit[1]);
	if (fv === undefined) return false;
	switch(lit[0]) {
	case "!=":
	  return fv != lit[2];
	case "==":
	  return fv == lit[2];
	default:
	  throw "unknown guard lit";
	}
      });
    });
  };

  /* Note: only activated (not disabled) input elements are
   * considered in form extraction */
  MenuItem.prototype.activate = function(v) {
    var di = this.get_input_elem();
    var d = this.get_elem();
    
    di.disabled = !v;
    
    d.style.display = v ? 'block': 'none';
    /* d.style.height = v ? 'auto' : 0 + 'px'; */
  };

  MenuItem.prototype.get_input_elem = function() {
    return this.input_elem;
  };

  MenuItem.prototype.get_text_field = function() {
    return this.input_elem;
  };

  MenuItem.prototype.get_elem = function() { /* TODO: wrong name */
    return this.elem;
  };

  MenuItem.prototype.set_flag_def_opt = function() {
    this.update_flag_with_value(this.def_opt); /* TODO: only if in_use? */
  };

  MenuItem.prototype.update_flag_with_index = function(i) {
    this.update_flag_with_value(this.options[i]);
  };

  MenuItem.prototype.update_flag_with_value = function(value) {
    this.selected_value = value;
  };

  MenuItem.prototype.get_flag_value = function() {
    return this.selected_value;
  };

  MenuItem.prototype.fill_elem = function() {
    // if (!this.in_use) return;
    if (this.is_combo) {
      var combo = this.get_input_elem();
      clear_combo(combo);
      for (var o = 0; o < this.options.length; o++) {
	combo.options[combo.length] = new Option(this.options[o], this.options[o]);
	if (this.options[o] == this.get_flag_value()) {
	  combo.selectedIndex = o;
	}
      }
    } else { // It has to be a text box!!!
      var textf = this.get_text_field();
      textf.value = this.def_opt;
    }
  };

  function clear_combo(combo) {
    for (var c = combo.length; c > 0; c--) {
      combo.options[c] = null;
    }
  }

  function menu_items_fill_elem() {
    for (var m = 0; m < menus.length; m++) {
      menus[m].fill_elem();
      menus[m].activate(menus[m].in_use);
    }
  }

  function menu_items_exec_guards() {
    for (var m = 0; m < menus.length; m++) {
      menus[m].exec_guard();
    }
  }

  function menu_items_update_def() {
    for (var m = 0; m < menus.length; m++) {
      menus[m].set_flag_def_opt();
    }
  }

  function menu_items_recompute_guards() {
    for (var m = 0; m < menus.length; m++) {
      var old = menus[m].in_use; 
      menus[m].exec_guard();
      if (old != menus[m].in_use) {
	menus[m].activate(!old);
      }
    }
  }

  function add_html(elm, text) {
    elm.appendChild(elements_from_string(text));
  }

  Playground.prototype._fill_itemlist = function(elm, itemlist) {
    itemlist.forEach((f) => {
      this._add_itemlink(elm, f);
    });
  };

  Playground.prototype._add_itemlink = function(elm, item) {
    var d = document.createElement('div');
    /* d.className = 'itemlink'; */
    var d2 = document.createElement('a');
    d2.href = "javascript:void(0)";
    var self = this;
    d2.onclick = function() { select_item.call(self, item); return false; };
    d2.appendChild(document.createTextNode(item));
    d.appendChild(d2);
    elm.appendChild(d);
  };

  function select_item(name) {
    modal_close();
    this.event['select_item'](name, function(res) {});
  }

  /* Show a modal dialog */
  Playground.prototype.show_modal = function(defs) {
    this._setup_form(modal_elm(), defs);
  };

  /* Set main toolbar */
  Playground.prototype.set_main_toolbar = function(button_list) {
    var w = document.getElementById('frame_buttons');
    this._setup_form(w, [
      { tag: "toolbar", value: button_list }
    ]);
  };

  /* Set contents of the given buffer */
  Playground.prototype.set_buf = function(buf, value) {
    this.buf_editor[buf].setValue(value);
  };

  /* Focus the given buffer */
  Playground.prototype.focus_buf = function(buf) {
    this.buf_editor[buf].focus();
  };

  /* Set mode (language) for the given buffer */
  Playground.prototype.buf_mode = function(buf, mode) {
    this.buf_editor[buf].setOption("mode", mode);
  };

  /* Set contents of the given HTML view */
  /* TODO: This is a hack -- we should not create editor in the first place! */
  Playground.prototype.set_html_view = function(buf, value) {
    var id = buf + "-buffer-contents"; /* TODO: not nice */
    var e = document.getElementById(id);
    e.innerHTML = value;
  };

  return Playground;
})();


