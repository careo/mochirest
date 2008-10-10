{application, mochirest,
 [{description, "mochirest"},
  {vsn, "0.01"},
  {modules, [
             mochirest,
             mochirest_app,
             mochirest_sup,
             mochirest_web,
             mochirest_deps
            ]},
  {registered, []},
  {mod, {mochirest_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
