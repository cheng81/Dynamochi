{application, dynamochi,
 [{description, "dynamochi"},
  {vsn, "0.01"},
  {modules, [
	dm_server,
    dynamochi,
    dynamochi_app,
    dynamochi_sup,
    dynamochi_web,
    dynamochi_deps
  ]},
  {registered, []},
  {mod, {dynamochi_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
