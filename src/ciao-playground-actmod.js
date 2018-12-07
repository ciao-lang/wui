/*
 *  ciao-playground-actmod.js
 *
 *  Binding of a Ciao Playground UI component to an active module.
 *
 *  Copyright (C) 2017 Jose F. Morales
 */

/* requires: ciao-actmod.js */
/* requires: ciao-playground-ui.js */

/* --------------------------------------------------------------------------- */

var init_ciao_playground_actmod = (function () {
  var pg;

  /* The ciao_pg_ui predicates */

  run_cmd['ciao_pg_ui.show_modal'] = function(data) {
    var args = data['args']; pg.show_modal(args[0]);
  };
  run_cmd['ciao_pg_ui.set_buf'] = function(data) {
    var args = data['args']; pg.set_buf(args[0], args[1]);
  };
  run_cmd['ciao_pg_ui.focus_buf'] = function(data) {
    var args = data['args']; pg.focus_buf(args[0]);
  };
  run_cmd['ciao_pg_ui.buf_mode'] = function(data) {
    var args = data['args']; pg.buf_mode(args[0], args[1]);
  };
  run_cmd['ciao_pg_ui.set_html_view'] = function(data) {
    var args = data['args']; pg.set_html_view(args[0], args[1]);
  };
  run_cmd['ciao_pg_ui.show_in_progress'] = function(data) {
    var args = data['args']; pg.show_in_progress(args[0]);
  };
  run_cmd['ciao_pg_ui.set_main_toolbar'] = function(data) {
    var args = data['args']; pg.set_main_toolbar(args[0]);
  };
  run_cmd['ciao_pg_ui.pg_setup'] = function(data) {
    /* Initial setup */
    /* TODO: allow different layouts */
    var args = data['args'];
    var loading = document.getElementById("loading");
    if (loading) loading.style.display = "none";
    pg.setup(args[0]);
    pg.show();
    status_on_console();
  };
  /* TODO: add another effect to replace a menu item value? */

  function status_on_console() {
    rpc_show_status = function(msg) {
      pg.show_status(msg);
    };
  }

  function rpc_call_init() {
    rpc_run(rpc_actmod, rpc_actmod+".__init__", {}, {}, function(res) {});
  }

  function init_ciao_playground_actmod(playground_id, actmod) {
    pg = new Playground(playground_id);
    pg.hide();

    rpc_actmod = actmod;

    pg.event['select_item'] = function(name, done) {
      /* TODO: add item kind, etc. define as another button */
      rpc_run(rpc_actmod, rpc_actmod+".load_file", {args: [name]}, {method: 'get'}, done);
    };
    pg.event['submit_cmd'] = function(submit_cmd, submit_opts, data, done) {
      var actmod;
      if (submit_opts['show_in_progress'] == true) {
	pg.show_in_progress(true); /* TODO: add from Prolog (as a larger continuation in modal, etc.) */
      }
      if (submit_opts['cmd_actmod'] !== undefined) {
	/* force actmod (instead of default) */
	actmod = submit_opts['cmd_actmod'];
      } else {
	actmod = rpc_actmod;
      }
      rpc_run(actmod, submit_cmd, data, {}, done);
    };

    rpc_call_init();
  }

  return init_ciao_playground_actmod;
})();
