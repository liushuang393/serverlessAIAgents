(function () {
  const u = document.createElement("link").relList;
  if (u && u.supports && u.supports("modulepreload")) return;
  for (const o of document.querySelectorAll('link[rel="modulepreload"]')) s(o);
  new MutationObserver((o) => {
    for (const f of o)
      if (f.type === "childList")
        for (const d of f.addedNodes)
          d.tagName === "LINK" && d.rel === "modulepreload" && s(d);
  }).observe(document, { childList: !0, subtree: !0 });
  function i(o) {
    const f = {};
    return (
      o.integrity && (f.integrity = o.integrity),
      o.referrerPolicy && (f.referrerPolicy = o.referrerPolicy),
      o.crossOrigin === "use-credentials"
        ? (f.credentials = "include")
        : o.crossOrigin === "anonymous"
          ? (f.credentials = "omit")
          : (f.credentials = "same-origin"),
      f
    );
  }
  function s(o) {
    if (o.ep) return;
    o.ep = !0;
    const f = i(o);
    fetch(o.href, f);
  }
})();
function of(l) {
  return l && l.__esModule && Object.prototype.hasOwnProperty.call(l, "default")
    ? l.default
    : l;
}
var To = { exports: {} },
  Ku = {};
/**
 * @license React
 * react-jsx-runtime.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */ var m0;
function a1() {
  if (m0) return Ku;
  m0 = 1;
  var l = Symbol.for("react.transitional.element"),
    u = Symbol.for("react.fragment");
  function i(s, o, f) {
    var d = null;
    if (
      (f !== void 0 && (d = "" + f),
      o.key !== void 0 && (d = "" + o.key),
      "key" in o)
    ) {
      f = {};
      for (var g in o) g !== "key" && (f[g] = o[g]);
    } else f = o;
    return (
      (o = f.ref),
      { $$typeof: l, type: s, key: d, ref: o !== void 0 ? o : null, props: f }
    );
  }
  return ((Ku.Fragment = u), (Ku.jsx = i), (Ku.jsxs = i), Ku);
}
var g0;
function u1() {
  return (g0 || ((g0 = 1), (To.exports = a1())), To.exports);
}
var A = u1(),
  zo = { exports: {} },
  St = {};
/**
 * @license React
 * react.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */ var y0;
function i1() {
  if (y0) return St;
  y0 = 1;
  var l = Symbol.for("react.transitional.element"),
    u = Symbol.for("react.portal"),
    i = Symbol.for("react.fragment"),
    s = Symbol.for("react.strict_mode"),
    o = Symbol.for("react.profiler"),
    f = Symbol.for("react.consumer"),
    d = Symbol.for("react.context"),
    g = Symbol.for("react.forward_ref"),
    y = Symbol.for("react.suspense"),
    m = Symbol.for("react.memo"),
    p = Symbol.for("react.lazy"),
    x = Symbol.for("react.activity"),
    _ = Symbol.iterator;
  function E(b) {
    return b === null || typeof b != "object"
      ? null
      : ((b = (_ && b[_]) || b["@@iterator"]),
        typeof b == "function" ? b : null);
  }
  var D = {
      isMounted: function () {
        return !1;
      },
      enqueueForceUpdate: function () {},
      enqueueReplaceState: function () {},
      enqueueSetState: function () {},
    },
    z = Object.assign,
    B = {};
  function N(b, M, $) {
    ((this.props = b),
      (this.context = M),
      (this.refs = B),
      (this.updater = $ || D));
  }
  ((N.prototype.isReactComponent = {}),
    (N.prototype.setState = function (b, M) {
      if (typeof b != "object" && typeof b != "function" && b != null)
        throw Error(
          "takes an object of state variables to update or a function which returns an object of state variables.",
        );
      this.updater.enqueueSetState(this, b, M, "setState");
    }),
    (N.prototype.forceUpdate = function (b) {
      this.updater.enqueueForceUpdate(this, b, "forceUpdate");
    }));
  function Y() {}
  Y.prototype = N.prototype;
  function G(b, M, $) {
    ((this.props = b),
      (this.context = M),
      (this.refs = B),
      (this.updater = $ || D));
  }
  var j = (G.prototype = new Y());
  ((j.constructor = G), z(j, N.prototype), (j.isPureReactComponent = !0));
  var W = Array.isArray;
  function I() {}
  var Q = { H: null, A: null, T: null, S: null },
    ut = Object.prototype.hasOwnProperty;
  function et(b, M, $) {
    var F = $.ref;
    return {
      $$typeof: l,
      type: b,
      key: M,
      ref: F !== void 0 ? F : null,
      props: $,
    };
  }
  function ct(b, M) {
    return et(b.type, M, b.props);
  }
  function at(b) {
    return typeof b == "object" && b !== null && b.$$typeof === l;
  }
  function st(b) {
    var M = { "=": "=0", ":": "=2" };
    return (
      "$" +
      b.replace(/[=:]/g, function ($) {
        return M[$];
      })
    );
  }
  var rt = /\/+/g;
  function w(b, M) {
    return typeof b == "object" && b !== null && b.key != null
      ? st("" + b.key)
      : M.toString(36);
  }
  function L(b) {
    switch (b.status) {
      case "fulfilled":
        return b.value;
      case "rejected":
        throw b.reason;
      default:
        switch (
          (typeof b.status == "string"
            ? b.then(I, I)
            : ((b.status = "pending"),
              b.then(
                function (M) {
                  b.status === "pending" &&
                    ((b.status = "fulfilled"), (b.value = M));
                },
                function (M) {
                  b.status === "pending" &&
                    ((b.status = "rejected"), (b.reason = M));
                },
              )),
          b.status)
        ) {
          case "fulfilled":
            return b.value;
          case "rejected":
            throw b.reason;
        }
    }
    throw b;
  }
  function S(b, M, $, F, it) {
    var ot = typeof b;
    (ot === "undefined" || ot === "boolean") && (b = null);
    var dt = !1;
    if (b === null) dt = !0;
    else
      switch (ot) {
        case "bigint":
        case "string":
        case "number":
          dt = !0;
          break;
        case "object":
          switch (b.$$typeof) {
            case l:
            case u:
              dt = !0;
              break;
            case p:
              return ((dt = b._init), S(dt(b._payload), M, $, F, it));
          }
      }
    if (dt)
      return (
        (it = it(b)),
        (dt = F === "" ? "." + w(b, 0) : F),
        W(it)
          ? (($ = ""),
            dt != null && ($ = dt.replace(rt, "$&/") + "/"),
            S(it, M, $, "", function (ht) {
              return ht;
            }))
          : it != null &&
            (at(it) &&
              (it = ct(
                it,
                $ +
                  (it.key == null || (b && b.key === it.key)
                    ? ""
                    : ("" + it.key).replace(rt, "$&/") + "/") +
                  dt,
              )),
            M.push(it)),
        1
      );
    dt = 0;
    var tt = F === "" ? "." : F + ":";
    if (W(b))
      for (var ft = 0; ft < b.length; ft++)
        ((F = b[ft]), (ot = tt + w(F, ft)), (dt += S(F, M, $, ot, it)));
    else if (((ft = E(b)), typeof ft == "function"))
      for (b = ft.call(b), ft = 0; !(F = b.next()).done; )
        ((F = F.value), (ot = tt + w(F, ft++)), (dt += S(F, M, $, ot, it)));
    else if (ot === "object") {
      if (typeof b.then == "function") return S(L(b), M, $, F, it);
      throw (
        (M = String(b)),
        Error(
          "Objects are not valid as a React child (found: " +
            (M === "[object Object]"
              ? "object with keys {" + Object.keys(b).join(", ") + "}"
              : M) +
            "). If you meant to render a collection of children, use an array instead.",
        )
      );
    }
    return dt;
  }
  function R(b, M, $) {
    if (b == null) return b;
    var F = [],
      it = 0;
    return (
      S(b, F, "", "", function (ot) {
        return M.call($, ot, it++);
      }),
      F
    );
  }
  function Z(b) {
    if (b._status === -1) {
      var M = b._result;
      ((M = M()),
        M.then(
          function ($) {
            (b._status === 0 || b._status === -1) &&
              ((b._status = 1), (b._result = $));
          },
          function ($) {
            (b._status === 0 || b._status === -1) &&
              ((b._status = 2), (b._result = $));
          },
        ),
        b._status === -1 && ((b._status = 0), (b._result = M)));
    }
    if (b._status === 1) return b._result.default;
    throw b._result;
  }
  var O =
      typeof reportError == "function"
        ? reportError
        : function (b) {
            if (
              typeof window == "object" &&
              typeof window.ErrorEvent == "function"
            ) {
              var M = new window.ErrorEvent("error", {
                bubbles: !0,
                cancelable: !0,
                message:
                  typeof b == "object" &&
                  b !== null &&
                  typeof b.message == "string"
                    ? String(b.message)
                    : String(b),
                error: b,
              });
              if (!window.dispatchEvent(M)) return;
            } else if (
              typeof process == "object" &&
              typeof process.emit == "function"
            ) {
              process.emit("uncaughtException", b);
              return;
            }
            console.error(b);
          },
    k = {
      map: R,
      forEach: function (b, M, $) {
        R(
          b,
          function () {
            M.apply(this, arguments);
          },
          $,
        );
      },
      count: function (b) {
        var M = 0;
        return (
          R(b, function () {
            M++;
          }),
          M
        );
      },
      toArray: function (b) {
        return (
          R(b, function (M) {
            return M;
          }) || []
        );
      },
      only: function (b) {
        if (!at(b))
          throw Error(
            "React.Children.only expected to receive a single React element child.",
          );
        return b;
      },
    };
  return (
    (St.Activity = x),
    (St.Children = k),
    (St.Component = N),
    (St.Fragment = i),
    (St.Profiler = o),
    (St.PureComponent = G),
    (St.StrictMode = s),
    (St.Suspense = y),
    (St.__CLIENT_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE = Q),
    (St.__COMPILER_RUNTIME = {
      __proto__: null,
      c: function (b) {
        return Q.H.useMemoCache(b);
      },
    }),
    (St.cache = function (b) {
      return function () {
        return b.apply(null, arguments);
      };
    }),
    (St.cacheSignal = function () {
      return null;
    }),
    (St.cloneElement = function (b, M, $) {
      if (b == null)
        throw Error(
          "The argument must be a React element, but you passed " + b + ".",
        );
      var F = z({}, b.props),
        it = b.key;
      if (M != null)
        for (ot in (M.key !== void 0 && (it = "" + M.key), M))
          !ut.call(M, ot) ||
            ot === "key" ||
            ot === "__self" ||
            ot === "__source" ||
            (ot === "ref" && M.ref === void 0) ||
            (F[ot] = M[ot]);
      var ot = arguments.length - 2;
      if (ot === 1) F.children = $;
      else if (1 < ot) {
        for (var dt = Array(ot), tt = 0; tt < ot; tt++)
          dt[tt] = arguments[tt + 2];
        F.children = dt;
      }
      return et(b.type, it, F);
    }),
    (St.createContext = function (b) {
      return (
        (b = {
          $$typeof: d,
          _currentValue: b,
          _currentValue2: b,
          _threadCount: 0,
          Provider: null,
          Consumer: null,
        }),
        (b.Provider = b),
        (b.Consumer = { $$typeof: f, _context: b }),
        b
      );
    }),
    (St.createElement = function (b, M, $) {
      var F,
        it = {},
        ot = null;
      if (M != null)
        for (F in (M.key !== void 0 && (ot = "" + M.key), M))
          ut.call(M, F) &&
            F !== "key" &&
            F !== "__self" &&
            F !== "__source" &&
            (it[F] = M[F]);
      var dt = arguments.length - 2;
      if (dt === 1) it.children = $;
      else if (1 < dt) {
        for (var tt = Array(dt), ft = 0; ft < dt; ft++)
          tt[ft] = arguments[ft + 2];
        it.children = tt;
      }
      if (b && b.defaultProps)
        for (F in ((dt = b.defaultProps), dt))
          it[F] === void 0 && (it[F] = dt[F]);
      return et(b, ot, it);
    }),
    (St.createRef = function () {
      return { current: null };
    }),
    (St.forwardRef = function (b) {
      return { $$typeof: g, render: b };
    }),
    (St.isValidElement = at),
    (St.lazy = function (b) {
      return { $$typeof: p, _payload: { _status: -1, _result: b }, _init: Z };
    }),
    (St.memo = function (b, M) {
      return { $$typeof: m, type: b, compare: M === void 0 ? null : M };
    }),
    (St.startTransition = function (b) {
      var M = Q.T,
        $ = {};
      Q.T = $;
      try {
        var F = b(),
          it = Q.S;
        (it !== null && it($, F),
          typeof F == "object" &&
            F !== null &&
            typeof F.then == "function" &&
            F.then(I, O));
      } catch (ot) {
        O(ot);
      } finally {
        (M !== null && $.types !== null && (M.types = $.types), (Q.T = M));
      }
    }),
    (St.unstable_useCacheRefresh = function () {
      return Q.H.useCacheRefresh();
    }),
    (St.use = function (b) {
      return Q.H.use(b);
    }),
    (St.useActionState = function (b, M, $) {
      return Q.H.useActionState(b, M, $);
    }),
    (St.useCallback = function (b, M) {
      return Q.H.useCallback(b, M);
    }),
    (St.useContext = function (b) {
      return Q.H.useContext(b);
    }),
    (St.useDebugValue = function () {}),
    (St.useDeferredValue = function (b, M) {
      return Q.H.useDeferredValue(b, M);
    }),
    (St.useEffect = function (b, M) {
      return Q.H.useEffect(b, M);
    }),
    (St.useEffectEvent = function (b) {
      return Q.H.useEffectEvent(b);
    }),
    (St.useId = function () {
      return Q.H.useId();
    }),
    (St.useImperativeHandle = function (b, M, $) {
      return Q.H.useImperativeHandle(b, M, $);
    }),
    (St.useInsertionEffect = function (b, M) {
      return Q.H.useInsertionEffect(b, M);
    }),
    (St.useLayoutEffect = function (b, M) {
      return Q.H.useLayoutEffect(b, M);
    }),
    (St.useMemo = function (b, M) {
      return Q.H.useMemo(b, M);
    }),
    (St.useOptimistic = function (b, M) {
      return Q.H.useOptimistic(b, M);
    }),
    (St.useReducer = function (b, M, $) {
      return Q.H.useReducer(b, M, $);
    }),
    (St.useRef = function (b) {
      return Q.H.useRef(b);
    }),
    (St.useState = function (b) {
      return Q.H.useState(b);
    }),
    (St.useSyncExternalStore = function (b, M, $) {
      return Q.H.useSyncExternalStore(b, M, $);
    }),
    (St.useTransition = function () {
      return Q.H.useTransition();
    }),
    (St.version = "19.2.4"),
    St
  );
}
var p0;
function fi() {
  return (p0 || ((p0 = 1), (zo.exports = i1())), zo.exports);
}
var H = fi();
const J = of(H);
var Mo = { exports: {} },
  $u = {},
  Co = { exports: {} },
  Do = {};
/**
 * @license React
 * scheduler.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */ var v0;
function c1() {
  return (
    v0 ||
      ((v0 = 1),
      (function (l) {
        function u(S, R) {
          var Z = S.length;
          S.push(R);
          t: for (; 0 < Z; ) {
            var O = (Z - 1) >>> 1,
              k = S[O];
            if (0 < o(k, R)) ((S[O] = R), (S[Z] = k), (Z = O));
            else break t;
          }
        }
        function i(S) {
          return S.length === 0 ? null : S[0];
        }
        function s(S) {
          if (S.length === 0) return null;
          var R = S[0],
            Z = S.pop();
          if (Z !== R) {
            S[0] = Z;
            t: for (var O = 0, k = S.length, b = k >>> 1; O < b; ) {
              var M = 2 * (O + 1) - 1,
                $ = S[M],
                F = M + 1,
                it = S[F];
              if (0 > o($, Z))
                F < k && 0 > o(it, $)
                  ? ((S[O] = it), (S[F] = Z), (O = F))
                  : ((S[O] = $), (S[M] = Z), (O = M));
              else if (F < k && 0 > o(it, Z))
                ((S[O] = it), (S[F] = Z), (O = F));
              else break t;
            }
          }
          return R;
        }
        function o(S, R) {
          var Z = S.sortIndex - R.sortIndex;
          return Z !== 0 ? Z : S.id - R.id;
        }
        if (
          ((l.unstable_now = void 0),
          typeof performance == "object" &&
            typeof performance.now == "function")
        ) {
          var f = performance;
          l.unstable_now = function () {
            return f.now();
          };
        } else {
          var d = Date,
            g = d.now();
          l.unstable_now = function () {
            return d.now() - g;
          };
        }
        var y = [],
          m = [],
          p = 1,
          x = null,
          _ = 3,
          E = !1,
          D = !1,
          z = !1,
          B = !1,
          N = typeof setTimeout == "function" ? setTimeout : null,
          Y = typeof clearTimeout == "function" ? clearTimeout : null,
          G = typeof setImmediate < "u" ? setImmediate : null;
        function j(S) {
          for (var R = i(m); R !== null; ) {
            if (R.callback === null) s(m);
            else if (R.startTime <= S)
              (s(m), (R.sortIndex = R.expirationTime), u(y, R));
            else break;
            R = i(m);
          }
        }
        function W(S) {
          if (((z = !1), j(S), !D))
            if (i(y) !== null) ((D = !0), I || ((I = !0), st()));
            else {
              var R = i(m);
              R !== null && L(W, R.startTime - S);
            }
        }
        var I = !1,
          Q = -1,
          ut = 5,
          et = -1;
        function ct() {
          return B ? !0 : !(l.unstable_now() - et < ut);
        }
        function at() {
          if (((B = !1), I)) {
            var S = l.unstable_now();
            et = S;
            var R = !0;
            try {
              t: {
                ((D = !1), z && ((z = !1), Y(Q), (Q = -1)), (E = !0));
                var Z = _;
                try {
                  e: {
                    for (
                      j(S), x = i(y);
                      x !== null && !(x.expirationTime > S && ct());
                    ) {
                      var O = x.callback;
                      if (typeof O == "function") {
                        ((x.callback = null), (_ = x.priorityLevel));
                        var k = O(x.expirationTime <= S);
                        if (((S = l.unstable_now()), typeof k == "function")) {
                          ((x.callback = k), j(S), (R = !0));
                          break e;
                        }
                        (x === i(y) && s(y), j(S));
                      } else s(y);
                      x = i(y);
                    }
                    if (x !== null) R = !0;
                    else {
                      var b = i(m);
                      (b !== null && L(W, b.startTime - S), (R = !1));
                    }
                  }
                  break t;
                } finally {
                  ((x = null), (_ = Z), (E = !1));
                }
                R = void 0;
              }
            } finally {
              R ? st() : (I = !1);
            }
          }
        }
        var st;
        if (typeof G == "function")
          st = function () {
            G(at);
          };
        else if (typeof MessageChannel < "u") {
          var rt = new MessageChannel(),
            w = rt.port2;
          ((rt.port1.onmessage = at),
            (st = function () {
              w.postMessage(null);
            }));
        } else
          st = function () {
            N(at, 0);
          };
        function L(S, R) {
          Q = N(function () {
            S(l.unstable_now());
          }, R);
        }
        ((l.unstable_IdlePriority = 5),
          (l.unstable_ImmediatePriority = 1),
          (l.unstable_LowPriority = 4),
          (l.unstable_NormalPriority = 3),
          (l.unstable_Profiling = null),
          (l.unstable_UserBlockingPriority = 2),
          (l.unstable_cancelCallback = function (S) {
            S.callback = null;
          }),
          (l.unstable_forceFrameRate = function (S) {
            0 > S || 125 < S
              ? console.error(
                  "forceFrameRate takes a positive int between 0 and 125, forcing frame rates higher than 125 fps is not supported",
                )
              : (ut = 0 < S ? Math.floor(1e3 / S) : 5);
          }),
          (l.unstable_getCurrentPriorityLevel = function () {
            return _;
          }),
          (l.unstable_next = function (S) {
            switch (_) {
              case 1:
              case 2:
              case 3:
                var R = 3;
                break;
              default:
                R = _;
            }
            var Z = _;
            _ = R;
            try {
              return S();
            } finally {
              _ = Z;
            }
          }),
          (l.unstable_requestPaint = function () {
            B = !0;
          }),
          (l.unstable_runWithPriority = function (S, R) {
            switch (S) {
              case 1:
              case 2:
              case 3:
              case 4:
              case 5:
                break;
              default:
                S = 3;
            }
            var Z = _;
            _ = S;
            try {
              return R();
            } finally {
              _ = Z;
            }
          }),
          (l.unstable_scheduleCallback = function (S, R, Z) {
            var O = l.unstable_now();
            switch (
              (typeof Z == "object" && Z !== null
                ? ((Z = Z.delay),
                  (Z = typeof Z == "number" && 0 < Z ? O + Z : O))
                : (Z = O),
              S)
            ) {
              case 1:
                var k = -1;
                break;
              case 2:
                k = 250;
                break;
              case 5:
                k = 1073741823;
                break;
              case 4:
                k = 1e4;
                break;
              default:
                k = 5e3;
            }
            return (
              (k = Z + k),
              (S = {
                id: p++,
                callback: R,
                priorityLevel: S,
                startTime: Z,
                expirationTime: k,
                sortIndex: -1,
              }),
              Z > O
                ? ((S.sortIndex = Z),
                  u(m, S),
                  i(y) === null &&
                    S === i(m) &&
                    (z ? (Y(Q), (Q = -1)) : (z = !0), L(W, Z - O)))
                : ((S.sortIndex = k),
                  u(y, S),
                  D || E || ((D = !0), I || ((I = !0), st()))),
              S
            );
          }),
          (l.unstable_shouldYield = ct),
          (l.unstable_wrapCallback = function (S) {
            var R = _;
            return function () {
              var Z = _;
              _ = R;
              try {
                return S.apply(this, arguments);
              } finally {
                _ = Z;
              }
            };
          }));
      })(Do)),
    Do
  );
}
var x0;
function s1() {
  return (x0 || ((x0 = 1), (Co.exports = c1())), Co.exports);
}
var Oo = { exports: {} },
  Ee = {};
/**
 * @license React
 * react-dom.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */ var b0;
function r1() {
  if (b0) return Ee;
  b0 = 1;
  var l = fi();
  function u(y) {
    var m = "https://react.dev/errors/" + y;
    if (1 < arguments.length) {
      m += "?args[]=" + encodeURIComponent(arguments[1]);
      for (var p = 2; p < arguments.length; p++)
        m += "&args[]=" + encodeURIComponent(arguments[p]);
    }
    return (
      "Minified React error #" +
      y +
      "; visit " +
      m +
      " for the full message or use the non-minified dev environment for full errors and additional helpful warnings."
    );
  }
  function i() {}
  var s = {
      d: {
        f: i,
        r: function () {
          throw Error(u(522));
        },
        D: i,
        C: i,
        L: i,
        m: i,
        X: i,
        S: i,
        M: i,
      },
      p: 0,
      findDOMNode: null,
    },
    o = Symbol.for("react.portal");
  function f(y, m, p) {
    var x =
      3 < arguments.length && arguments[3] !== void 0 ? arguments[3] : null;
    return {
      $$typeof: o,
      key: x == null ? null : "" + x,
      children: y,
      containerInfo: m,
      implementation: p,
    };
  }
  var d = l.__CLIENT_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE;
  function g(y, m) {
    if (y === "font") return "";
    if (typeof m == "string") return m === "use-credentials" ? m : "";
  }
  return (
    (Ee.__DOM_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE = s),
    (Ee.createPortal = function (y, m) {
      var p =
        2 < arguments.length && arguments[2] !== void 0 ? arguments[2] : null;
      if (!m || (m.nodeType !== 1 && m.nodeType !== 9 && m.nodeType !== 11))
        throw Error(u(299));
      return f(y, m, null, p);
    }),
    (Ee.flushSync = function (y) {
      var m = d.T,
        p = s.p;
      try {
        if (((d.T = null), (s.p = 2), y)) return y();
      } finally {
        ((d.T = m), (s.p = p), s.d.f());
      }
    }),
    (Ee.preconnect = function (y, m) {
      typeof y == "string" &&
        (m
          ? ((m = m.crossOrigin),
            (m =
              typeof m == "string"
                ? m === "use-credentials"
                  ? m
                  : ""
                : void 0))
          : (m = null),
        s.d.C(y, m));
    }),
    (Ee.prefetchDNS = function (y) {
      typeof y == "string" && s.d.D(y);
    }),
    (Ee.preinit = function (y, m) {
      if (typeof y == "string" && m && typeof m.as == "string") {
        var p = m.as,
          x = g(p, m.crossOrigin),
          _ = typeof m.integrity == "string" ? m.integrity : void 0,
          E = typeof m.fetchPriority == "string" ? m.fetchPriority : void 0;
        p === "style"
          ? s.d.S(y, typeof m.precedence == "string" ? m.precedence : void 0, {
              crossOrigin: x,
              integrity: _,
              fetchPriority: E,
            })
          : p === "script" &&
            s.d.X(y, {
              crossOrigin: x,
              integrity: _,
              fetchPriority: E,
              nonce: typeof m.nonce == "string" ? m.nonce : void 0,
            });
      }
    }),
    (Ee.preinitModule = function (y, m) {
      if (typeof y == "string")
        if (typeof m == "object" && m !== null) {
          if (m.as == null || m.as === "script") {
            var p = g(m.as, m.crossOrigin);
            s.d.M(y, {
              crossOrigin: p,
              integrity: typeof m.integrity == "string" ? m.integrity : void 0,
              nonce: typeof m.nonce == "string" ? m.nonce : void 0,
            });
          }
        } else m == null && s.d.M(y);
    }),
    (Ee.preload = function (y, m) {
      if (
        typeof y == "string" &&
        typeof m == "object" &&
        m !== null &&
        typeof m.as == "string"
      ) {
        var p = m.as,
          x = g(p, m.crossOrigin);
        s.d.L(y, p, {
          crossOrigin: x,
          integrity: typeof m.integrity == "string" ? m.integrity : void 0,
          nonce: typeof m.nonce == "string" ? m.nonce : void 0,
          type: typeof m.type == "string" ? m.type : void 0,
          fetchPriority:
            typeof m.fetchPriority == "string" ? m.fetchPriority : void 0,
          referrerPolicy:
            typeof m.referrerPolicy == "string" ? m.referrerPolicy : void 0,
          imageSrcSet:
            typeof m.imageSrcSet == "string" ? m.imageSrcSet : void 0,
          imageSizes: typeof m.imageSizes == "string" ? m.imageSizes : void 0,
          media: typeof m.media == "string" ? m.media : void 0,
        });
      }
    }),
    (Ee.preloadModule = function (y, m) {
      if (typeof y == "string")
        if (m) {
          var p = g(m.as, m.crossOrigin);
          s.d.m(y, {
            as: typeof m.as == "string" && m.as !== "script" ? m.as : void 0,
            crossOrigin: p,
            integrity: typeof m.integrity == "string" ? m.integrity : void 0,
          });
        } else s.d.m(y);
    }),
    (Ee.requestFormReset = function (y) {
      s.d.r(y);
    }),
    (Ee.unstable_batchedUpdates = function (y, m) {
      return y(m);
    }),
    (Ee.useFormState = function (y, m, p) {
      return d.H.useFormState(y, m, p);
    }),
    (Ee.useFormStatus = function () {
      return d.H.useHostTransitionStatus();
    }),
    (Ee.version = "19.2.4"),
    Ee
  );
}
var S0;
function bg() {
  if (S0) return Oo.exports;
  S0 = 1;
  function l() {
    if (
      !(
        typeof __REACT_DEVTOOLS_GLOBAL_HOOK__ > "u" ||
        typeof __REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE != "function"
      )
    )
      try {
        __REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE(l);
      } catch (u) {
        console.error(u);
      }
  }
  return (l(), (Oo.exports = r1()), Oo.exports);
}
/**
 * @license React
 * react-dom-client.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */ var E0;
function o1() {
  if (E0) return $u;
  E0 = 1;
  var l = s1(),
    u = fi(),
    i = bg();
  function s(t) {
    var e = "https://react.dev/errors/" + t;
    if (1 < arguments.length) {
      e += "?args[]=" + encodeURIComponent(arguments[1]);
      for (var n = 2; n < arguments.length; n++)
        e += "&args[]=" + encodeURIComponent(arguments[n]);
    }
    return (
      "Minified React error #" +
      t +
      "; visit " +
      e +
      " for the full message or use the non-minified dev environment for full errors and additional helpful warnings."
    );
  }
  function o(t) {
    return !(!t || (t.nodeType !== 1 && t.nodeType !== 9 && t.nodeType !== 11));
  }
  function f(t) {
    var e = t,
      n = t;
    if (t.alternate) for (; e.return; ) e = e.return;
    else {
      t = e;
      do ((e = t), (e.flags & 4098) !== 0 && (n = e.return), (t = e.return));
      while (t);
    }
    return e.tag === 3 ? n : null;
  }
  function d(t) {
    if (t.tag === 13) {
      var e = t.memoizedState;
      if (
        (e === null && ((t = t.alternate), t !== null && (e = t.memoizedState)),
        e !== null)
      )
        return e.dehydrated;
    }
    return null;
  }
  function g(t) {
    if (t.tag === 31) {
      var e = t.memoizedState;
      if (
        (e === null && ((t = t.alternate), t !== null && (e = t.memoizedState)),
        e !== null)
      )
        return e.dehydrated;
    }
    return null;
  }
  function y(t) {
    if (f(t) !== t) throw Error(s(188));
  }
  function m(t) {
    var e = t.alternate;
    if (!e) {
      if (((e = f(t)), e === null)) throw Error(s(188));
      return e !== t ? null : t;
    }
    for (var n = t, a = e; ; ) {
      var c = n.return;
      if (c === null) break;
      var r = c.alternate;
      if (r === null) {
        if (((a = c.return), a !== null)) {
          n = a;
          continue;
        }
        break;
      }
      if (c.child === r.child) {
        for (r = c.child; r; ) {
          if (r === n) return (y(c), t);
          if (r === a) return (y(c), e);
          r = r.sibling;
        }
        throw Error(s(188));
      }
      if (n.return !== a.return) ((n = c), (a = r));
      else {
        for (var h = !1, v = c.child; v; ) {
          if (v === n) {
            ((h = !0), (n = c), (a = r));
            break;
          }
          if (v === a) {
            ((h = !0), (a = c), (n = r));
            break;
          }
          v = v.sibling;
        }
        if (!h) {
          for (v = r.child; v; ) {
            if (v === n) {
              ((h = !0), (n = r), (a = c));
              break;
            }
            if (v === a) {
              ((h = !0), (a = r), (n = c));
              break;
            }
            v = v.sibling;
          }
          if (!h) throw Error(s(189));
        }
      }
      if (n.alternate !== a) throw Error(s(190));
    }
    if (n.tag !== 3) throw Error(s(188));
    return n.stateNode.current === n ? t : e;
  }
  function p(t) {
    var e = t.tag;
    if (e === 5 || e === 26 || e === 27 || e === 6) return t;
    for (t = t.child; t !== null; ) {
      if (((e = p(t)), e !== null)) return e;
      t = t.sibling;
    }
    return null;
  }
  var x = Object.assign,
    _ = Symbol.for("react.element"),
    E = Symbol.for("react.transitional.element"),
    D = Symbol.for("react.portal"),
    z = Symbol.for("react.fragment"),
    B = Symbol.for("react.strict_mode"),
    N = Symbol.for("react.profiler"),
    Y = Symbol.for("react.consumer"),
    G = Symbol.for("react.context"),
    j = Symbol.for("react.forward_ref"),
    W = Symbol.for("react.suspense"),
    I = Symbol.for("react.suspense_list"),
    Q = Symbol.for("react.memo"),
    ut = Symbol.for("react.lazy"),
    et = Symbol.for("react.activity"),
    ct = Symbol.for("react.memo_cache_sentinel"),
    at = Symbol.iterator;
  function st(t) {
    return t === null || typeof t != "object"
      ? null
      : ((t = (at && t[at]) || t["@@iterator"]),
        typeof t == "function" ? t : null);
  }
  var rt = Symbol.for("react.client.reference");
  function w(t) {
    if (t == null) return null;
    if (typeof t == "function")
      return t.$$typeof === rt ? null : t.displayName || t.name || null;
    if (typeof t == "string") return t;
    switch (t) {
      case z:
        return "Fragment";
      case N:
        return "Profiler";
      case B:
        return "StrictMode";
      case W:
        return "Suspense";
      case I:
        return "SuspenseList";
      case et:
        return "Activity";
    }
    if (typeof t == "object")
      switch (t.$$typeof) {
        case D:
          return "Portal";
        case G:
          return t.displayName || "Context";
        case Y:
          return (t._context.displayName || "Context") + ".Consumer";
        case j:
          var e = t.render;
          return (
            (t = t.displayName),
            t ||
              ((t = e.displayName || e.name || ""),
              (t = t !== "" ? "ForwardRef(" + t + ")" : "ForwardRef")),
            t
          );
        case Q:
          return (
            (e = t.displayName || null),
            e !== null ? e : w(t.type) || "Memo"
          );
        case ut:
          ((e = t._payload), (t = t._init));
          try {
            return w(t(e));
          } catch {}
      }
    return null;
  }
  var L = Array.isArray,
    S = u.__CLIENT_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE,
    R = i.__DOM_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE,
    Z = { pending: !1, data: null, method: null, action: null },
    O = [],
    k = -1;
  function b(t) {
    return { current: t };
  }
  function M(t) {
    0 > k || ((t.current = O[k]), (O[k] = null), k--);
  }
  function $(t, e) {
    (k++, (O[k] = t.current), (t.current = e));
  }
  var F = b(null),
    it = b(null),
    ot = b(null),
    dt = b(null);
  function tt(t, e) {
    switch (($(ot, e), $(it, t), $(F, null), e.nodeType)) {
      case 9:
      case 11:
        t = (t = e.documentElement) && (t = t.namespaceURI) ? Bm(t) : 0;
        break;
      default:
        if (((t = e.tagName), (e = e.namespaceURI)))
          ((e = Bm(e)), (t = Ym(e, t)));
        else
          switch (t) {
            case "svg":
              t = 1;
              break;
            case "math":
              t = 2;
              break;
            default:
              t = 0;
          }
    }
    (M(F), $(F, t));
  }
  function ft() {
    (M(F), M(it), M(ot));
  }
  function ht(t) {
    t.memoizedState !== null && $(dt, t);
    var e = F.current,
      n = Ym(e, t.type);
    e !== n && ($(it, t), $(F, n));
  }
  function bt(t) {
    (it.current === t && (M(F), M(it)),
      dt.current === t && (M(dt), (Gu._currentValue = Z)));
  }
  var Nt, ne;
  function Vt(t) {
    if (Nt === void 0)
      try {
        throw Error();
      } catch (n) {
        var e = n.stack.trim().match(/\n( *(at )?)/);
        ((Nt = (e && e[1]) || ""),
          (ne =
            -1 <
            n.stack.indexOf(`
    at`)
              ? " (<anonymous>)"
              : -1 < n.stack.indexOf("@")
                ? "@unknown:0:0"
                : ""));
      }
    return (
      `
` +
      Nt +
      t +
      ne
    );
  }
  var Lt = !1;
  function se(t, e) {
    if (!t || Lt) return "";
    Lt = !0;
    var n = Error.prepareStackTrace;
    Error.prepareStackTrace = void 0;
    try {
      var a = {
        DetermineComponentFrameRoot: function () {
          try {
            if (e) {
              var lt = function () {
                throw Error();
              };
              if (
                (Object.defineProperty(lt.prototype, "props", {
                  set: function () {
                    throw Error();
                  },
                }),
                typeof Reflect == "object" && Reflect.construct)
              ) {
                try {
                  Reflect.construct(lt, []);
                } catch (K) {
                  var V = K;
                }
                Reflect.construct(t, [], lt);
              } else {
                try {
                  lt.call();
                } catch (K) {
                  V = K;
                }
                t.call(lt.prototype);
              }
            } else {
              try {
                throw Error();
              } catch (K) {
                V = K;
              }
              (lt = t()) &&
                typeof lt.catch == "function" &&
                lt.catch(function () {});
            }
          } catch (K) {
            if (K && V && typeof K.stack == "string") return [K.stack, V.stack];
          }
          return [null, null];
        },
      };
      a.DetermineComponentFrameRoot.displayName = "DetermineComponentFrameRoot";
      var c = Object.getOwnPropertyDescriptor(
        a.DetermineComponentFrameRoot,
        "name",
      );
      c &&
        c.configurable &&
        Object.defineProperty(a.DetermineComponentFrameRoot, "name", {
          value: "DetermineComponentFrameRoot",
        });
      var r = a.DetermineComponentFrameRoot(),
        h = r[0],
        v = r[1];
      if (h && v) {
        var T = h.split(`
`),
          X = v.split(`
`);
        for (
          c = a = 0;
          a < T.length && !T[a].includes("DetermineComponentFrameRoot");
        )
          a++;
        for (; c < X.length && !X[c].includes("DetermineComponentFrameRoot"); )
          c++;
        if (a === T.length || c === X.length)
          for (
            a = T.length - 1, c = X.length - 1;
            1 <= a && 0 <= c && T[a] !== X[c];
          )
            c--;
        for (; 1 <= a && 0 <= c; a--, c--)
          if (T[a] !== X[c]) {
            if (a !== 1 || c !== 1)
              do
                if ((a--, c--, 0 > c || T[a] !== X[c])) {
                  var P =
                    `
` + T[a].replace(" at new ", " at ");
                  return (
                    t.displayName &&
                      P.includes("<anonymous>") &&
                      (P = P.replace("<anonymous>", t.displayName)),
                    P
                  );
                }
              while (1 <= a && 0 <= c);
            break;
          }
      }
    } finally {
      ((Lt = !1), (Error.prepareStackTrace = n));
    }
    return (n = t ? t.displayName || t.name : "") ? Vt(n) : "";
  }
  function Dt(t, e) {
    switch (t.tag) {
      case 26:
      case 27:
      case 5:
        return Vt(t.type);
      case 16:
        return Vt("Lazy");
      case 13:
        return t.child !== e && e !== null
          ? Vt("Suspense Fallback")
          : Vt("Suspense");
      case 19:
        return Vt("SuspenseList");
      case 0:
      case 15:
        return se(t.type, !1);
      case 11:
        return se(t.type.render, !1);
      case 1:
        return se(t.type, !0);
      case 31:
        return Vt("Activity");
      default:
        return "";
    }
  }
  function gt(t) {
    try {
      var e = "",
        n = null;
      do ((e += Dt(t, n)), (n = t), (t = t.return));
      while (t);
      return e;
    } catch (a) {
      return (
        `
Error generating stack: ` +
        a.message +
        `
` +
        a.stack
      );
    }
  }
  var It = Object.prototype.hasOwnProperty,
    Oe = l.unstable_scheduleCallback,
    Sn = l.unstable_cancelCallback,
    $n = l.unstable_shouldYield,
    Jn = l.unstable_requestPaint,
    Kt = l.unstable_now,
    Wn = l.unstable_getCurrentPriorityLevel,
    fn = l.unstable_ImmediatePriority,
    dn = l.unstable_UserBlockingPriority,
    nn = l.unstable_NormalPriority,
    Fn = l.unstable_LowPriority,
    In = l.unstable_IdlePriority,
    de = l.log,
    Re = l.unstable_setDisableYieldValue,
    He = null,
    Pt = null;
  function Se(t) {
    if (
      (typeof de == "function" && Re(t),
      Pt && typeof Pt.setStrictMode == "function")
    )
      try {
        Pt.setStrictMode(He, t);
      } catch {}
  }
  var $t = Math.clz32 ? Math.clz32 : Pa,
    Ge = Math.log,
    Dl = Math.LN2;
  function Pa(t) {
    return ((t >>>= 0), t === 0 ? 32 : (31 - ((Ge(t) / Dl) | 0)) | 0);
  }
  var hn = 256,
    Pn = 262144,
    Ol = 4194304;
  function _e(t) {
    var e = t & 42;
    if (e !== 0) return e;
    switch (t & -t) {
      case 1:
        return 1;
      case 2:
        return 2;
      case 4:
        return 4;
      case 8:
        return 8;
      case 16:
        return 16;
      case 32:
        return 32;
      case 64:
        return 64;
      case 128:
        return 128;
      case 256:
      case 512:
      case 1024:
      case 2048:
      case 4096:
      case 8192:
      case 16384:
      case 32768:
      case 65536:
      case 131072:
        return t & 261888;
      case 262144:
      case 524288:
      case 1048576:
      case 2097152:
        return t & 3932160;
      case 4194304:
      case 8388608:
      case 16777216:
      case 33554432:
        return t & 62914560;
      case 67108864:
        return 67108864;
      case 134217728:
        return 134217728;
      case 268435456:
        return 268435456;
      case 536870912:
        return 536870912;
      case 1073741824:
        return 0;
      default:
        return t;
    }
  }
  function ia(t, e, n) {
    var a = t.pendingLanes;
    if (a === 0) return 0;
    var c = 0,
      r = t.suspendedLanes,
      h = t.pingedLanes;
    t = t.warmLanes;
    var v = a & 134217727;
    return (
      v !== 0
        ? ((a = v & ~r),
          a !== 0
            ? (c = _e(a))
            : ((h &= v),
              h !== 0
                ? (c = _e(h))
                : n || ((n = v & ~t), n !== 0 && (c = _e(n)))))
        : ((v = a & ~r),
          v !== 0
            ? (c = _e(v))
            : h !== 0
              ? (c = _e(h))
              : n || ((n = a & ~t), n !== 0 && (c = _e(n)))),
      c === 0
        ? 0
        : e !== 0 &&
            e !== c &&
            (e & r) === 0 &&
            ((r = c & -c),
            (n = e & -e),
            r >= n || (r === 32 && (n & 4194048) !== 0))
          ? e
          : c
    );
  }
  function Rl(t, e) {
    return (t.pendingLanes & ~(t.suspendedLanes & ~t.pingedLanes) & e) === 0;
  }
  function ms(t, e) {
    switch (t) {
      case 1:
      case 2:
      case 4:
      case 8:
      case 64:
        return e + 250;
      case 16:
      case 32:
      case 128:
      case 256:
      case 512:
      case 1024:
      case 2048:
      case 4096:
      case 8192:
      case 16384:
      case 32768:
      case 65536:
      case 131072:
      case 262144:
      case 524288:
      case 1048576:
      case 2097152:
        return e + 5e3;
      case 4194304:
      case 8388608:
      case 16777216:
      case 33554432:
        return -1;
      case 67108864:
      case 134217728:
      case 268435456:
      case 536870912:
      case 1073741824:
        return -1;
      default:
        return -1;
    }
  }
  function gi() {
    var t = Ol;
    return ((Ol <<= 1), (Ol & 62914560) === 0 && (Ol = 4194304), t);
  }
  function tu(t) {
    for (var e = [], n = 0; 31 > n; n++) e.push(t);
    return e;
  }
  function Hl(t, e) {
    ((t.pendingLanes |= e),
      e !== 268435456 &&
        ((t.suspendedLanes = 0), (t.pingedLanes = 0), (t.warmLanes = 0)));
  }
  function gs(t, e, n, a, c, r) {
    var h = t.pendingLanes;
    ((t.pendingLanes = n),
      (t.suspendedLanes = 0),
      (t.pingedLanes = 0),
      (t.warmLanes = 0),
      (t.expiredLanes &= n),
      (t.entangledLanes &= n),
      (t.errorRecoveryDisabledLanes &= n),
      (t.shellSuspendCounter = 0));
    var v = t.entanglements,
      T = t.expirationTimes,
      X = t.hiddenUpdates;
    for (n = h & ~n; 0 < n; ) {
      var P = 31 - $t(n),
        lt = 1 << P;
      ((v[P] = 0), (T[P] = -1));
      var V = X[P];
      if (V !== null)
        for (X[P] = null, P = 0; P < V.length; P++) {
          var K = V[P];
          K !== null && (K.lane &= -536870913);
        }
      n &= ~lt;
    }
    (a !== 0 && yi(t, a, 0),
      r !== 0 && c === 0 && t.tag !== 0 && (t.suspendedLanes |= r & ~(h & ~e)));
  }
  function yi(t, e, n) {
    ((t.pendingLanes |= e), (t.suspendedLanes &= ~e));
    var a = 31 - $t(e);
    ((t.entangledLanes |= e),
      (t.entanglements[a] = t.entanglements[a] | 1073741824 | (n & 261930)));
  }
  function pi(t, e) {
    var n = (t.entangledLanes |= e);
    for (t = t.entanglements; n; ) {
      var a = 31 - $t(n),
        c = 1 << a;
      ((c & e) | (t[a] & e) && (t[a] |= e), (n &= ~c));
    }
  }
  function vi(t, e) {
    var n = e & -e;
    return (
      (n = (n & 42) !== 0 ? 1 : eu(n)),
      (n & (t.suspendedLanes | e)) !== 0 ? 0 : n
    );
  }
  function eu(t) {
    switch (t) {
      case 2:
        t = 1;
        break;
      case 8:
        t = 4;
        break;
      case 32:
        t = 16;
        break;
      case 256:
      case 512:
      case 1024:
      case 2048:
      case 4096:
      case 8192:
      case 16384:
      case 32768:
      case 65536:
      case 131072:
      case 262144:
      case 524288:
      case 1048576:
      case 2097152:
      case 4194304:
      case 8388608:
      case 16777216:
      case 33554432:
        t = 128;
        break;
      case 268435456:
        t = 134217728;
        break;
      default:
        t = 0;
    }
    return t;
  }
  function nu(t) {
    return (
      (t &= -t),
      2 < t ? (8 < t ? ((t & 134217727) !== 0 ? 32 : 268435456) : 8) : 2
    );
  }
  function xi() {
    var t = R.p;
    return t !== 0 ? t : ((t = window.event), t === void 0 ? 32 : c0(t.type));
  }
  function bi(t, e) {
    var n = R.p;
    try {
      return ((R.p = t), e());
    } finally {
      R.p = n;
    }
  }
  var mn = Math.random().toString(36).slice(2),
    re = "__reactFiber$" + mn,
    ge = "__reactProps$" + mn,
    tl = "__reactContainer$" + mn,
    lu = "__reactEvents$" + mn,
    ys = "__reactListeners$" + mn,
    ps = "__reactHandles$" + mn,
    Si = "__reactResources$" + mn,
    jl = "__reactMarker$" + mn;
  function au(t) {
    (delete t[re], delete t[ge], delete t[lu], delete t[ys], delete t[ps]);
  }
  function el(t) {
    var e = t[re];
    if (e) return e;
    for (var n = t.parentNode; n; ) {
      if ((e = n[tl] || n[re])) {
        if (
          ((n = e.alternate),
          e.child !== null || (n !== null && n.child !== null))
        )
          for (t = Qm(t); t !== null; ) {
            if ((n = t[re])) return n;
            t = Qm(t);
          }
        return e;
      }
      ((t = n), (n = t.parentNode));
    }
    return null;
  }
  function En(t) {
    if ((t = t[re] || t[tl])) {
      var e = t.tag;
      if (
        e === 5 ||
        e === 6 ||
        e === 13 ||
        e === 31 ||
        e === 26 ||
        e === 27 ||
        e === 3
      )
        return t;
    }
    return null;
  }
  function Ul(t) {
    var e = t.tag;
    if (e === 5 || e === 26 || e === 27 || e === 6) return t.stateNode;
    throw Error(s(33));
  }
  function nl(t) {
    var e = t[Si];
    return (
      e ||
        (e = t[Si] =
          { hoistableStyles: new Map(), hoistableScripts: new Map() }),
      e
    );
  }
  function le(t) {
    t[jl] = !0;
  }
  var Ei = new Set(),
    _i = {};
  function _n(t, e) {
    (wn(t, e), wn(t + "Capture", e));
  }
  function wn(t, e) {
    for (_i[t] = e, t = 0; t < e.length; t++) Ei.add(e[t]);
  }
  var vs = RegExp(
      "^[:A-Z_a-z\\u00C0-\\u00D6\\u00D8-\\u00F6\\u00F8-\\u02FF\\u0370-\\u037D\\u037F-\\u1FFF\\u200C-\\u200D\\u2070-\\u218F\\u2C00-\\u2FEF\\u3001-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFFD][:A-Z_a-z\\u00C0-\\u00D6\\u00D8-\\u00F6\\u00F8-\\u02FF\\u0370-\\u037D\\u037F-\\u1FFF\\u200C-\\u200D\\u2070-\\u218F\\u2C00-\\u2FEF\\u3001-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFFD\\-.0-9\\u00B7\\u0300-\\u036F\\u203F-\\u2040]*$",
    ),
    wi = {},
    Ni = {};
  function uu(t) {
    return It.call(Ni, t)
      ? !0
      : It.call(wi, t)
        ? !1
        : vs.test(t)
          ? (Ni[t] = !0)
          : ((wi[t] = !0), !1);
  }
  function Ai(t, e, n) {
    if (uu(e))
      if (n === null) t.removeAttribute(e);
      else {
        switch (typeof n) {
          case "undefined":
          case "function":
          case "symbol":
            t.removeAttribute(e);
            return;
          case "boolean":
            var a = e.toLowerCase().slice(0, 5);
            if (a !== "data-" && a !== "aria-") {
              t.removeAttribute(e);
              return;
            }
        }
        t.setAttribute(e, "" + n);
      }
  }
  function Ti(t, e, n) {
    if (n === null) t.removeAttribute(e);
    else {
      switch (typeof n) {
        case "undefined":
        case "function":
        case "symbol":
        case "boolean":
          t.removeAttribute(e);
          return;
      }
      t.setAttribute(e, "" + n);
    }
  }
  function Nn(t, e, n, a) {
    if (a === null) t.removeAttribute(n);
    else {
      switch (typeof a) {
        case "undefined":
        case "function":
        case "symbol":
        case "boolean":
          t.removeAttribute(n);
          return;
      }
      t.setAttributeNS(e, n, "" + a);
    }
  }
  function Ze(t) {
    switch (typeof t) {
      case "bigint":
      case "boolean":
      case "number":
      case "string":
      case "undefined":
        return t;
      case "object":
        return t;
      default:
        return "";
    }
  }
  function Mf(t) {
    var e = t.type;
    return (
      (t = t.nodeName) &&
      t.toLowerCase() === "input" &&
      (e === "checkbox" || e === "radio")
    );
  }
  function Fy(t, e, n) {
    var a = Object.getOwnPropertyDescriptor(t.constructor.prototype, e);
    if (
      !t.hasOwnProperty(e) &&
      typeof a < "u" &&
      typeof a.get == "function" &&
      typeof a.set == "function"
    ) {
      var c = a.get,
        r = a.set;
      return (
        Object.defineProperty(t, e, {
          configurable: !0,
          get: function () {
            return c.call(this);
          },
          set: function (h) {
            ((n = "" + h), r.call(this, h));
          },
        }),
        Object.defineProperty(t, e, { enumerable: a.enumerable }),
        {
          getValue: function () {
            return n;
          },
          setValue: function (h) {
            n = "" + h;
          },
          stopTracking: function () {
            ((t._valueTracker = null), delete t[e]);
          },
        }
      );
    }
  }
  function xs(t) {
    if (!t._valueTracker) {
      var e = Mf(t) ? "checked" : "value";
      t._valueTracker = Fy(t, e, "" + t[e]);
    }
  }
  function Cf(t) {
    if (!t) return !1;
    var e = t._valueTracker;
    if (!e) return !0;
    var n = e.getValue(),
      a = "";
    return (
      t && (a = Mf(t) ? (t.checked ? "true" : "false") : t.value),
      (t = a),
      t !== n ? (e.setValue(t), !0) : !1
    );
  }
  function zi(t) {
    if (
      ((t = t || (typeof document < "u" ? document : void 0)), typeof t > "u")
    )
      return null;
    try {
      return t.activeElement || t.body;
    } catch {
      return t.body;
    }
  }
  var Iy = /[\n"\\]/g;
  function Qe(t) {
    return t.replace(Iy, function (e) {
      return "\\" + e.charCodeAt(0).toString(16) + " ";
    });
  }
  function bs(t, e, n, a, c, r, h, v) {
    ((t.name = ""),
      h != null &&
      typeof h != "function" &&
      typeof h != "symbol" &&
      typeof h != "boolean"
        ? (t.type = h)
        : t.removeAttribute("type"),
      e != null
        ? h === "number"
          ? ((e === 0 && t.value === "") || t.value != e) &&
            (t.value = "" + Ze(e))
          : t.value !== "" + Ze(e) && (t.value = "" + Ze(e))
        : (h !== "submit" && h !== "reset") || t.removeAttribute("value"),
      e != null
        ? Ss(t, h, Ze(e))
        : n != null
          ? Ss(t, h, Ze(n))
          : a != null && t.removeAttribute("value"),
      c == null && r != null && (t.defaultChecked = !!r),
      c != null &&
        (t.checked = c && typeof c != "function" && typeof c != "symbol"),
      v != null &&
      typeof v != "function" &&
      typeof v != "symbol" &&
      typeof v != "boolean"
        ? (t.name = "" + Ze(v))
        : t.removeAttribute("name"));
  }
  function Df(t, e, n, a, c, r, h, v) {
    if (
      (r != null &&
        typeof r != "function" &&
        typeof r != "symbol" &&
        typeof r != "boolean" &&
        (t.type = r),
      e != null || n != null)
    ) {
      if (!((r !== "submit" && r !== "reset") || e != null)) {
        xs(t);
        return;
      }
      ((n = n != null ? "" + Ze(n) : ""),
        (e = e != null ? "" + Ze(e) : n),
        v || e === t.value || (t.value = e),
        (t.defaultValue = e));
    }
    ((a = a ?? c),
      (a = typeof a != "function" && typeof a != "symbol" && !!a),
      (t.checked = v ? t.checked : !!a),
      (t.defaultChecked = !!a),
      h != null &&
        typeof h != "function" &&
        typeof h != "symbol" &&
        typeof h != "boolean" &&
        (t.name = h),
      xs(t));
  }
  function Ss(t, e, n) {
    (e === "number" && zi(t.ownerDocument) === t) ||
      t.defaultValue === "" + n ||
      (t.defaultValue = "" + n);
  }
  function ca(t, e, n, a) {
    if (((t = t.options), e)) {
      e = {};
      for (var c = 0; c < n.length; c++) e["$" + n[c]] = !0;
      for (n = 0; n < t.length; n++)
        ((c = e.hasOwnProperty("$" + t[n].value)),
          t[n].selected !== c && (t[n].selected = c),
          c && a && (t[n].defaultSelected = !0));
    } else {
      for (n = "" + Ze(n), e = null, c = 0; c < t.length; c++) {
        if (t[c].value === n) {
          ((t[c].selected = !0), a && (t[c].defaultSelected = !0));
          return;
        }
        e !== null || t[c].disabled || (e = t[c]);
      }
      e !== null && (e.selected = !0);
    }
  }
  function Of(t, e, n) {
    if (
      e != null &&
      ((e = "" + Ze(e)), e !== t.value && (t.value = e), n == null)
    ) {
      t.defaultValue !== e && (t.defaultValue = e);
      return;
    }
    t.defaultValue = n != null ? "" + Ze(n) : "";
  }
  function Rf(t, e, n, a) {
    if (e == null) {
      if (a != null) {
        if (n != null) throw Error(s(92));
        if (L(a)) {
          if (1 < a.length) throw Error(s(93));
          a = a[0];
        }
        n = a;
      }
      (n == null && (n = ""), (e = n));
    }
    ((n = Ze(e)),
      (t.defaultValue = n),
      (a = t.textContent),
      a === n && a !== "" && a !== null && (t.value = a),
      xs(t));
  }
  function sa(t, e) {
    if (e) {
      var n = t.firstChild;
      if (n && n === t.lastChild && n.nodeType === 3) {
        n.nodeValue = e;
        return;
      }
    }
    t.textContent = e;
  }
  var Py = new Set(
    "animationIterationCount aspectRatio borderImageOutset borderImageSlice borderImageWidth boxFlex boxFlexGroup boxOrdinalGroup columnCount columns flex flexGrow flexPositive flexShrink flexNegative flexOrder gridArea gridRow gridRowEnd gridRowSpan gridRowStart gridColumn gridColumnEnd gridColumnSpan gridColumnStart fontWeight lineClamp lineHeight opacity order orphans scale tabSize widows zIndex zoom fillOpacity floodOpacity stopOpacity strokeDasharray strokeDashoffset strokeMiterlimit strokeOpacity strokeWidth MozAnimationIterationCount MozBoxFlex MozBoxFlexGroup MozLineClamp msAnimationIterationCount msFlex msZoom msFlexGrow msFlexNegative msFlexOrder msFlexPositive msFlexShrink msGridColumn msGridColumnSpan msGridRow msGridRowSpan WebkitAnimationIterationCount WebkitBoxFlex WebKitBoxFlexGroup WebkitBoxOrdinalGroup WebkitColumnCount WebkitColumns WebkitFlex WebkitFlexGrow WebkitFlexPositive WebkitFlexShrink WebkitLineClamp".split(
      " ",
    ),
  );
  function Hf(t, e, n) {
    var a = e.indexOf("--") === 0;
    n == null || typeof n == "boolean" || n === ""
      ? a
        ? t.setProperty(e, "")
        : e === "float"
          ? (t.cssFloat = "")
          : (t[e] = "")
      : a
        ? t.setProperty(e, n)
        : typeof n != "number" || n === 0 || Py.has(e)
          ? e === "float"
            ? (t.cssFloat = n)
            : (t[e] = ("" + n).trim())
          : (t[e] = n + "px");
  }
  function jf(t, e, n) {
    if (e != null && typeof e != "object") throw Error(s(62));
    if (((t = t.style), n != null)) {
      for (var a in n)
        !n.hasOwnProperty(a) ||
          (e != null && e.hasOwnProperty(a)) ||
          (a.indexOf("--") === 0
            ? t.setProperty(a, "")
            : a === "float"
              ? (t.cssFloat = "")
              : (t[a] = ""));
      for (var c in e)
        ((a = e[c]), e.hasOwnProperty(c) && n[c] !== a && Hf(t, c, a));
    } else for (var r in e) e.hasOwnProperty(r) && Hf(t, r, e[r]);
  }
  function Es(t) {
    if (t.indexOf("-") === -1) return !1;
    switch (t) {
      case "annotation-xml":
      case "color-profile":
      case "font-face":
      case "font-face-src":
      case "font-face-uri":
      case "font-face-format":
      case "font-face-name":
      case "missing-glyph":
        return !1;
      default:
        return !0;
    }
  }
  var tp = new Map([
      ["acceptCharset", "accept-charset"],
      ["htmlFor", "for"],
      ["httpEquiv", "http-equiv"],
      ["crossOrigin", "crossorigin"],
      ["accentHeight", "accent-height"],
      ["alignmentBaseline", "alignment-baseline"],
      ["arabicForm", "arabic-form"],
      ["baselineShift", "baseline-shift"],
      ["capHeight", "cap-height"],
      ["clipPath", "clip-path"],
      ["clipRule", "clip-rule"],
      ["colorInterpolation", "color-interpolation"],
      ["colorInterpolationFilters", "color-interpolation-filters"],
      ["colorProfile", "color-profile"],
      ["colorRendering", "color-rendering"],
      ["dominantBaseline", "dominant-baseline"],
      ["enableBackground", "enable-background"],
      ["fillOpacity", "fill-opacity"],
      ["fillRule", "fill-rule"],
      ["floodColor", "flood-color"],
      ["floodOpacity", "flood-opacity"],
      ["fontFamily", "font-family"],
      ["fontSize", "font-size"],
      ["fontSizeAdjust", "font-size-adjust"],
      ["fontStretch", "font-stretch"],
      ["fontStyle", "font-style"],
      ["fontVariant", "font-variant"],
      ["fontWeight", "font-weight"],
      ["glyphName", "glyph-name"],
      ["glyphOrientationHorizontal", "glyph-orientation-horizontal"],
      ["glyphOrientationVertical", "glyph-orientation-vertical"],
      ["horizAdvX", "horiz-adv-x"],
      ["horizOriginX", "horiz-origin-x"],
      ["imageRendering", "image-rendering"],
      ["letterSpacing", "letter-spacing"],
      ["lightingColor", "lighting-color"],
      ["markerEnd", "marker-end"],
      ["markerMid", "marker-mid"],
      ["markerStart", "marker-start"],
      ["overlinePosition", "overline-position"],
      ["overlineThickness", "overline-thickness"],
      ["paintOrder", "paint-order"],
      ["panose-1", "panose-1"],
      ["pointerEvents", "pointer-events"],
      ["renderingIntent", "rendering-intent"],
      ["shapeRendering", "shape-rendering"],
      ["stopColor", "stop-color"],
      ["stopOpacity", "stop-opacity"],
      ["strikethroughPosition", "strikethrough-position"],
      ["strikethroughThickness", "strikethrough-thickness"],
      ["strokeDasharray", "stroke-dasharray"],
      ["strokeDashoffset", "stroke-dashoffset"],
      ["strokeLinecap", "stroke-linecap"],
      ["strokeLinejoin", "stroke-linejoin"],
      ["strokeMiterlimit", "stroke-miterlimit"],
      ["strokeOpacity", "stroke-opacity"],
      ["strokeWidth", "stroke-width"],
      ["textAnchor", "text-anchor"],
      ["textDecoration", "text-decoration"],
      ["textRendering", "text-rendering"],
      ["transformOrigin", "transform-origin"],
      ["underlinePosition", "underline-position"],
      ["underlineThickness", "underline-thickness"],
      ["unicodeBidi", "unicode-bidi"],
      ["unicodeRange", "unicode-range"],
      ["unitsPerEm", "units-per-em"],
      ["vAlphabetic", "v-alphabetic"],
      ["vHanging", "v-hanging"],
      ["vIdeographic", "v-ideographic"],
      ["vMathematical", "v-mathematical"],
      ["vectorEffect", "vector-effect"],
      ["vertAdvY", "vert-adv-y"],
      ["vertOriginX", "vert-origin-x"],
      ["vertOriginY", "vert-origin-y"],
      ["wordSpacing", "word-spacing"],
      ["writingMode", "writing-mode"],
      ["xmlnsXlink", "xmlns:xlink"],
      ["xHeight", "x-height"],
    ]),
    ep =
      /^[\u0000-\u001F ]*j[\r\n\t]*a[\r\n\t]*v[\r\n\t]*a[\r\n\t]*s[\r\n\t]*c[\r\n\t]*r[\r\n\t]*i[\r\n\t]*p[\r\n\t]*t[\r\n\t]*:/i;
  function Mi(t) {
    return ep.test("" + t)
      ? "javascript:throw new Error('React has blocked a javascript: URL as a security precaution.')"
      : t;
  }
  function An() {}
  var _s = null;
  function ws(t) {
    return (
      (t = t.target || t.srcElement || window),
      t.correspondingUseElement && (t = t.correspondingUseElement),
      t.nodeType === 3 ? t.parentNode : t
    );
  }
  var ra = null,
    oa = null;
  function Uf(t) {
    var e = En(t);
    if (e && (t = e.stateNode)) {
      var n = t[ge] || null;
      t: switch (((t = e.stateNode), e.type)) {
        case "input":
          if (
            (bs(
              t,
              n.value,
              n.defaultValue,
              n.defaultValue,
              n.checked,
              n.defaultChecked,
              n.type,
              n.name,
            ),
            (e = n.name),
            n.type === "radio" && e != null)
          ) {
            for (n = t; n.parentNode; ) n = n.parentNode;
            for (
              n = n.querySelectorAll(
                'input[name="' + Qe("" + e) + '"][type="radio"]',
              ),
                e = 0;
              e < n.length;
              e++
            ) {
              var a = n[e];
              if (a !== t && a.form === t.form) {
                var c = a[ge] || null;
                if (!c) throw Error(s(90));
                bs(
                  a,
                  c.value,
                  c.defaultValue,
                  c.defaultValue,
                  c.checked,
                  c.defaultChecked,
                  c.type,
                  c.name,
                );
              }
            }
            for (e = 0; e < n.length; e++)
              ((a = n[e]), a.form === t.form && Cf(a));
          }
          break t;
        case "textarea":
          Of(t, n.value, n.defaultValue);
          break t;
        case "select":
          ((e = n.value), e != null && ca(t, !!n.multiple, e, !1));
      }
    }
  }
  var Ns = !1;
  function Bf(t, e, n) {
    if (Ns) return t(e, n);
    Ns = !0;
    try {
      var a = t(e);
      return a;
    } finally {
      if (
        ((Ns = !1),
        (ra !== null || oa !== null) &&
          (yc(), ra && ((e = ra), (t = oa), (oa = ra = null), Uf(e), t)))
      )
        for (e = 0; e < t.length; e++) Uf(t[e]);
    }
  }
  function iu(t, e) {
    var n = t.stateNode;
    if (n === null) return null;
    var a = n[ge] || null;
    if (a === null) return null;
    n = a[e];
    t: switch (e) {
      case "onClick":
      case "onClickCapture":
      case "onDoubleClick":
      case "onDoubleClickCapture":
      case "onMouseDown":
      case "onMouseDownCapture":
      case "onMouseMove":
      case "onMouseMoveCapture":
      case "onMouseUp":
      case "onMouseUpCapture":
      case "onMouseEnter":
        ((a = !a.disabled) ||
          ((t = t.type),
          (a = !(
            t === "button" ||
            t === "input" ||
            t === "select" ||
            t === "textarea"
          ))),
          (t = !a));
        break t;
      default:
        t = !1;
    }
    if (t) return null;
    if (n && typeof n != "function") throw Error(s(231, e, typeof n));
    return n;
  }
  var Tn = !(
      typeof window > "u" ||
      typeof window.document > "u" ||
      typeof window.document.createElement > "u"
    ),
    As = !1;
  if (Tn)
    try {
      var cu = {};
      (Object.defineProperty(cu, "passive", {
        get: function () {
          As = !0;
        },
      }),
        window.addEventListener("test", cu, cu),
        window.removeEventListener("test", cu, cu));
    } catch {
      As = !1;
    }
  var ll = null,
    Ts = null,
    Ci = null;
  function Yf() {
    if (Ci) return Ci;
    var t,
      e = Ts,
      n = e.length,
      a,
      c = "value" in ll ? ll.value : ll.textContent,
      r = c.length;
    for (t = 0; t < n && e[t] === c[t]; t++);
    var h = n - t;
    for (a = 1; a <= h && e[n - a] === c[r - a]; a++);
    return (Ci = c.slice(t, 1 < a ? 1 - a : void 0));
  }
  function Di(t) {
    var e = t.keyCode;
    return (
      "charCode" in t
        ? ((t = t.charCode), t === 0 && e === 13 && (t = 13))
        : (t = e),
      t === 10 && (t = 13),
      32 <= t || t === 13 ? t : 0
    );
  }
  function Oi() {
    return !0;
  }
  function qf() {
    return !1;
  }
  function Ne(t) {
    function e(n, a, c, r, h) {
      ((this._reactName = n),
        (this._targetInst = c),
        (this.type = a),
        (this.nativeEvent = r),
        (this.target = h),
        (this.currentTarget = null));
      for (var v in t)
        t.hasOwnProperty(v) && ((n = t[v]), (this[v] = n ? n(r) : r[v]));
      return (
        (this.isDefaultPrevented = (
          r.defaultPrevented != null ? r.defaultPrevented : r.returnValue === !1
        )
          ? Oi
          : qf),
        (this.isPropagationStopped = qf),
        this
      );
    }
    return (
      x(e.prototype, {
        preventDefault: function () {
          this.defaultPrevented = !0;
          var n = this.nativeEvent;
          n &&
            (n.preventDefault
              ? n.preventDefault()
              : typeof n.returnValue != "unknown" && (n.returnValue = !1),
            (this.isDefaultPrevented = Oi));
        },
        stopPropagation: function () {
          var n = this.nativeEvent;
          n &&
            (n.stopPropagation
              ? n.stopPropagation()
              : typeof n.cancelBubble != "unknown" && (n.cancelBubble = !0),
            (this.isPropagationStopped = Oi));
        },
        persist: function () {},
        isPersistent: Oi,
      }),
      e
    );
  }
  var Bl = {
      eventPhase: 0,
      bubbles: 0,
      cancelable: 0,
      timeStamp: function (t) {
        return t.timeStamp || Date.now();
      },
      defaultPrevented: 0,
      isTrusted: 0,
    },
    Ri = Ne(Bl),
    su = x({}, Bl, { view: 0, detail: 0 }),
    np = Ne(su),
    zs,
    Ms,
    ru,
    Hi = x({}, su, {
      screenX: 0,
      screenY: 0,
      clientX: 0,
      clientY: 0,
      pageX: 0,
      pageY: 0,
      ctrlKey: 0,
      shiftKey: 0,
      altKey: 0,
      metaKey: 0,
      getModifierState: Ds,
      button: 0,
      buttons: 0,
      relatedTarget: function (t) {
        return t.relatedTarget === void 0
          ? t.fromElement === t.srcElement
            ? t.toElement
            : t.fromElement
          : t.relatedTarget;
      },
      movementX: function (t) {
        return "movementX" in t
          ? t.movementX
          : (t !== ru &&
              (ru && t.type === "mousemove"
                ? ((zs = t.screenX - ru.screenX), (Ms = t.screenY - ru.screenY))
                : (Ms = zs = 0),
              (ru = t)),
            zs);
      },
      movementY: function (t) {
        return "movementY" in t ? t.movementY : Ms;
      },
    }),
    Xf = Ne(Hi),
    lp = x({}, Hi, { dataTransfer: 0 }),
    ap = Ne(lp),
    up = x({}, su, { relatedTarget: 0 }),
    Cs = Ne(up),
    ip = x({}, Bl, { animationName: 0, elapsedTime: 0, pseudoElement: 0 }),
    cp = Ne(ip),
    sp = x({}, Bl, {
      clipboardData: function (t) {
        return "clipboardData" in t ? t.clipboardData : window.clipboardData;
      },
    }),
    rp = Ne(sp),
    op = x({}, Bl, { data: 0 }),
    Vf = Ne(op),
    fp = {
      Esc: "Escape",
      Spacebar: " ",
      Left: "ArrowLeft",
      Up: "ArrowUp",
      Right: "ArrowRight",
      Down: "ArrowDown",
      Del: "Delete",
      Win: "OS",
      Menu: "ContextMenu",
      Apps: "ContextMenu",
      Scroll: "ScrollLock",
      MozPrintableKey: "Unidentified",
    },
    dp = {
      8: "Backspace",
      9: "Tab",
      12: "Clear",
      13: "Enter",
      16: "Shift",
      17: "Control",
      18: "Alt",
      19: "Pause",
      20: "CapsLock",
      27: "Escape",
      32: " ",
      33: "PageUp",
      34: "PageDown",
      35: "End",
      36: "Home",
      37: "ArrowLeft",
      38: "ArrowUp",
      39: "ArrowRight",
      40: "ArrowDown",
      45: "Insert",
      46: "Delete",
      112: "F1",
      113: "F2",
      114: "F3",
      115: "F4",
      116: "F5",
      117: "F6",
      118: "F7",
      119: "F8",
      120: "F9",
      121: "F10",
      122: "F11",
      123: "F12",
      144: "NumLock",
      145: "ScrollLock",
      224: "Meta",
    },
    hp = {
      Alt: "altKey",
      Control: "ctrlKey",
      Meta: "metaKey",
      Shift: "shiftKey",
    };
  function mp(t) {
    var e = this.nativeEvent;
    return e.getModifierState
      ? e.getModifierState(t)
      : (t = hp[t])
        ? !!e[t]
        : !1;
  }
  function Ds() {
    return mp;
  }
  var gp = x({}, su, {
      key: function (t) {
        if (t.key) {
          var e = fp[t.key] || t.key;
          if (e !== "Unidentified") return e;
        }
        return t.type === "keypress"
          ? ((t = Di(t)), t === 13 ? "Enter" : String.fromCharCode(t))
          : t.type === "keydown" || t.type === "keyup"
            ? dp[t.keyCode] || "Unidentified"
            : "";
      },
      code: 0,
      location: 0,
      ctrlKey: 0,
      shiftKey: 0,
      altKey: 0,
      metaKey: 0,
      repeat: 0,
      locale: 0,
      getModifierState: Ds,
      charCode: function (t) {
        return t.type === "keypress" ? Di(t) : 0;
      },
      keyCode: function (t) {
        return t.type === "keydown" || t.type === "keyup" ? t.keyCode : 0;
      },
      which: function (t) {
        return t.type === "keypress"
          ? Di(t)
          : t.type === "keydown" || t.type === "keyup"
            ? t.keyCode
            : 0;
      },
    }),
    yp = Ne(gp),
    pp = x({}, Hi, {
      pointerId: 0,
      width: 0,
      height: 0,
      pressure: 0,
      tangentialPressure: 0,
      tiltX: 0,
      tiltY: 0,
      twist: 0,
      pointerType: 0,
      isPrimary: 0,
    }),
    Lf = Ne(pp),
    vp = x({}, su, {
      touches: 0,
      targetTouches: 0,
      changedTouches: 0,
      altKey: 0,
      metaKey: 0,
      ctrlKey: 0,
      shiftKey: 0,
      getModifierState: Ds,
    }),
    xp = Ne(vp),
    bp = x({}, Bl, { propertyName: 0, elapsedTime: 0, pseudoElement: 0 }),
    Sp = Ne(bp),
    Ep = x({}, Hi, {
      deltaX: function (t) {
        return "deltaX" in t
          ? t.deltaX
          : "wheelDeltaX" in t
            ? -t.wheelDeltaX
            : 0;
      },
      deltaY: function (t) {
        return "deltaY" in t
          ? t.deltaY
          : "wheelDeltaY" in t
            ? -t.wheelDeltaY
            : "wheelDelta" in t
              ? -t.wheelDelta
              : 0;
      },
      deltaZ: 0,
      deltaMode: 0,
    }),
    _p = Ne(Ep),
    wp = x({}, Bl, { newState: 0, oldState: 0 }),
    Np = Ne(wp),
    Ap = [9, 13, 27, 32],
    Os = Tn && "CompositionEvent" in window,
    ou = null;
  Tn && "documentMode" in document && (ou = document.documentMode);
  var Tp = Tn && "TextEvent" in window && !ou,
    Gf = Tn && (!Os || (ou && 8 < ou && 11 >= ou)),
    Zf = " ",
    Qf = !1;
  function kf(t, e) {
    switch (t) {
      case "keyup":
        return Ap.indexOf(e.keyCode) !== -1;
      case "keydown":
        return e.keyCode !== 229;
      case "keypress":
      case "mousedown":
      case "focusout":
        return !0;
      default:
        return !1;
    }
  }
  function Kf(t) {
    return (
      (t = t.detail),
      typeof t == "object" && "data" in t ? t.data : null
    );
  }
  var fa = !1;
  function zp(t, e) {
    switch (t) {
      case "compositionend":
        return Kf(e);
      case "keypress":
        return e.which !== 32 ? null : ((Qf = !0), Zf);
      case "textInput":
        return ((t = e.data), t === Zf && Qf ? null : t);
      default:
        return null;
    }
  }
  function Mp(t, e) {
    if (fa)
      return t === "compositionend" || (!Os && kf(t, e))
        ? ((t = Yf()), (Ci = Ts = ll = null), (fa = !1), t)
        : null;
    switch (t) {
      case "paste":
        return null;
      case "keypress":
        if (!(e.ctrlKey || e.altKey || e.metaKey) || (e.ctrlKey && e.altKey)) {
          if (e.char && 1 < e.char.length) return e.char;
          if (e.which) return String.fromCharCode(e.which);
        }
        return null;
      case "compositionend":
        return Gf && e.locale !== "ko" ? null : e.data;
      default:
        return null;
    }
  }
  var Cp = {
    color: !0,
    date: !0,
    datetime: !0,
    "datetime-local": !0,
    email: !0,
    month: !0,
    number: !0,
    password: !0,
    range: !0,
    search: !0,
    tel: !0,
    text: !0,
    time: !0,
    url: !0,
    week: !0,
  };
  function $f(t) {
    var e = t && t.nodeName && t.nodeName.toLowerCase();
    return e === "input" ? !!Cp[t.type] : e === "textarea";
  }
  function Jf(t, e, n, a) {
    (ra ? (oa ? oa.push(a) : (oa = [a])) : (ra = a),
      (e = _c(e, "onChange")),
      0 < e.length &&
        ((n = new Ri("onChange", "change", null, n, a)),
        t.push({ event: n, listeners: e })));
  }
  var fu = null,
    du = null;
  function Dp(t) {
    Dm(t, 0);
  }
  function ji(t) {
    var e = Ul(t);
    if (Cf(e)) return t;
  }
  function Wf(t, e) {
    if (t === "change") return e;
  }
  var Ff = !1;
  if (Tn) {
    var Rs;
    if (Tn) {
      var Hs = "oninput" in document;
      if (!Hs) {
        var If = document.createElement("div");
        (If.setAttribute("oninput", "return;"),
          (Hs = typeof If.oninput == "function"));
      }
      Rs = Hs;
    } else Rs = !1;
    Ff = Rs && (!document.documentMode || 9 < document.documentMode);
  }
  function Pf() {
    fu && (fu.detachEvent("onpropertychange", td), (du = fu = null));
  }
  function td(t) {
    if (t.propertyName === "value" && ji(du)) {
      var e = [];
      (Jf(e, du, t, ws(t)), Bf(Dp, e));
    }
  }
  function Op(t, e, n) {
    t === "focusin"
      ? (Pf(), (fu = e), (du = n), fu.attachEvent("onpropertychange", td))
      : t === "focusout" && Pf();
  }
  function Rp(t) {
    if (t === "selectionchange" || t === "keyup" || t === "keydown")
      return ji(du);
  }
  function Hp(t, e) {
    if (t === "click") return ji(e);
  }
  function jp(t, e) {
    if (t === "input" || t === "change") return ji(e);
  }
  function Up(t, e) {
    return (t === e && (t !== 0 || 1 / t === 1 / e)) || (t !== t && e !== e);
  }
  var je = typeof Object.is == "function" ? Object.is : Up;
  function hu(t, e) {
    if (je(t, e)) return !0;
    if (
      typeof t != "object" ||
      t === null ||
      typeof e != "object" ||
      e === null
    )
      return !1;
    var n = Object.keys(t),
      a = Object.keys(e);
    if (n.length !== a.length) return !1;
    for (a = 0; a < n.length; a++) {
      var c = n[a];
      if (!It.call(e, c) || !je(t[c], e[c])) return !1;
    }
    return !0;
  }
  function ed(t) {
    for (; t && t.firstChild; ) t = t.firstChild;
    return t;
  }
  function nd(t, e) {
    var n = ed(t);
    t = 0;
    for (var a; n; ) {
      if (n.nodeType === 3) {
        if (((a = t + n.textContent.length), t <= e && a >= e))
          return { node: n, offset: e - t };
        t = a;
      }
      t: {
        for (; n; ) {
          if (n.nextSibling) {
            n = n.nextSibling;
            break t;
          }
          n = n.parentNode;
        }
        n = void 0;
      }
      n = ed(n);
    }
  }
  function ld(t, e) {
    return t && e
      ? t === e
        ? !0
        : t && t.nodeType === 3
          ? !1
          : e && e.nodeType === 3
            ? ld(t, e.parentNode)
            : "contains" in t
              ? t.contains(e)
              : t.compareDocumentPosition
                ? !!(t.compareDocumentPosition(e) & 16)
                : !1
      : !1;
  }
  function ad(t) {
    t =
      t != null &&
      t.ownerDocument != null &&
      t.ownerDocument.defaultView != null
        ? t.ownerDocument.defaultView
        : window;
    for (var e = zi(t.document); e instanceof t.HTMLIFrameElement; ) {
      try {
        var n = typeof e.contentWindow.location.href == "string";
      } catch {
        n = !1;
      }
      if (n) t = e.contentWindow;
      else break;
      e = zi(t.document);
    }
    return e;
  }
  function js(t) {
    var e = t && t.nodeName && t.nodeName.toLowerCase();
    return (
      e &&
      ((e === "input" &&
        (t.type === "text" ||
          t.type === "search" ||
          t.type === "tel" ||
          t.type === "url" ||
          t.type === "password")) ||
        e === "textarea" ||
        t.contentEditable === "true")
    );
  }
  var Bp = Tn && "documentMode" in document && 11 >= document.documentMode,
    da = null,
    Us = null,
    mu = null,
    Bs = !1;
  function ud(t, e, n) {
    var a =
      n.window === n ? n.document : n.nodeType === 9 ? n : n.ownerDocument;
    Bs ||
      da == null ||
      da !== zi(a) ||
      ((a = da),
      "selectionStart" in a && js(a)
        ? (a = { start: a.selectionStart, end: a.selectionEnd })
        : ((a = (
            (a.ownerDocument && a.ownerDocument.defaultView) ||
            window
          ).getSelection()),
          (a = {
            anchorNode: a.anchorNode,
            anchorOffset: a.anchorOffset,
            focusNode: a.focusNode,
            focusOffset: a.focusOffset,
          })),
      (mu && hu(mu, a)) ||
        ((mu = a),
        (a = _c(Us, "onSelect")),
        0 < a.length &&
          ((e = new Ri("onSelect", "select", null, e, n)),
          t.push({ event: e, listeners: a }),
          (e.target = da))));
  }
  function Yl(t, e) {
    var n = {};
    return (
      (n[t.toLowerCase()] = e.toLowerCase()),
      (n["Webkit" + t] = "webkit" + e),
      (n["Moz" + t] = "moz" + e),
      n
    );
  }
  var ha = {
      animationend: Yl("Animation", "AnimationEnd"),
      animationiteration: Yl("Animation", "AnimationIteration"),
      animationstart: Yl("Animation", "AnimationStart"),
      transitionrun: Yl("Transition", "TransitionRun"),
      transitionstart: Yl("Transition", "TransitionStart"),
      transitioncancel: Yl("Transition", "TransitionCancel"),
      transitionend: Yl("Transition", "TransitionEnd"),
    },
    Ys = {},
    id = {};
  Tn &&
    ((id = document.createElement("div").style),
    "AnimationEvent" in window ||
      (delete ha.animationend.animation,
      delete ha.animationiteration.animation,
      delete ha.animationstart.animation),
    "TransitionEvent" in window || delete ha.transitionend.transition);
  function ql(t) {
    if (Ys[t]) return Ys[t];
    if (!ha[t]) return t;
    var e = ha[t],
      n;
    for (n in e) if (e.hasOwnProperty(n) && n in id) return (Ys[t] = e[n]);
    return t;
  }
  var cd = ql("animationend"),
    sd = ql("animationiteration"),
    rd = ql("animationstart"),
    Yp = ql("transitionrun"),
    qp = ql("transitionstart"),
    Xp = ql("transitioncancel"),
    od = ql("transitionend"),
    fd = new Map(),
    qs =
      "abort auxClick beforeToggle cancel canPlay canPlayThrough click close contextMenu copy cut drag dragEnd dragEnter dragExit dragLeave dragOver dragStart drop durationChange emptied encrypted ended error gotPointerCapture input invalid keyDown keyPress keyUp load loadedData loadedMetadata loadStart lostPointerCapture mouseDown mouseMove mouseOut mouseOver mouseUp paste pause play playing pointerCancel pointerDown pointerMove pointerOut pointerOver pointerUp progress rateChange reset resize seeked seeking stalled submit suspend timeUpdate touchCancel touchEnd touchStart volumeChange scroll toggle touchMove waiting wheel".split(
        " ",
      );
  qs.push("scrollEnd");
  function ln(t, e) {
    (fd.set(t, e), _n(e, [t]));
  }
  var Ui =
      typeof reportError == "function"
        ? reportError
        : function (t) {
            if (
              typeof window == "object" &&
              typeof window.ErrorEvent == "function"
            ) {
              var e = new window.ErrorEvent("error", {
                bubbles: !0,
                cancelable: !0,
                message:
                  typeof t == "object" &&
                  t !== null &&
                  typeof t.message == "string"
                    ? String(t.message)
                    : String(t),
                error: t,
              });
              if (!window.dispatchEvent(e)) return;
            } else if (
              typeof process == "object" &&
              typeof process.emit == "function"
            ) {
              process.emit("uncaughtException", t);
              return;
            }
            console.error(t);
          },
    ke = [],
    ma = 0,
    Xs = 0;
  function Bi() {
    for (var t = ma, e = (Xs = ma = 0); e < t; ) {
      var n = ke[e];
      ke[e++] = null;
      var a = ke[e];
      ke[e++] = null;
      var c = ke[e];
      ke[e++] = null;
      var r = ke[e];
      if (((ke[e++] = null), a !== null && c !== null)) {
        var h = a.pending;
        (h === null ? (c.next = c) : ((c.next = h.next), (h.next = c)),
          (a.pending = c));
      }
      r !== 0 && dd(n, c, r);
    }
  }
  function Yi(t, e, n, a) {
    ((ke[ma++] = t),
      (ke[ma++] = e),
      (ke[ma++] = n),
      (ke[ma++] = a),
      (Xs |= a),
      (t.lanes |= a),
      (t = t.alternate),
      t !== null && (t.lanes |= a));
  }
  function Vs(t, e, n, a) {
    return (Yi(t, e, n, a), qi(t));
  }
  function Xl(t, e) {
    return (Yi(t, null, null, e), qi(t));
  }
  function dd(t, e, n) {
    t.lanes |= n;
    var a = t.alternate;
    a !== null && (a.lanes |= n);
    for (var c = !1, r = t.return; r !== null; )
      ((r.childLanes |= n),
        (a = r.alternate),
        a !== null && (a.childLanes |= n),
        r.tag === 22 &&
          ((t = r.stateNode), t === null || t._visibility & 1 || (c = !0)),
        (t = r),
        (r = r.return));
    return t.tag === 3
      ? ((r = t.stateNode),
        c &&
          e !== null &&
          ((c = 31 - $t(n)),
          (t = r.hiddenUpdates),
          (a = t[c]),
          a === null ? (t[c] = [e]) : a.push(e),
          (e.lane = n | 536870912)),
        r)
      : null;
  }
  function qi(t) {
    if (50 < Uu) throw ((Uu = 0), (Wr = null), Error(s(185)));
    for (var e = t.return; e !== null; ) ((t = e), (e = t.return));
    return t.tag === 3 ? t.stateNode : null;
  }
  var ga = {};
  function Vp(t, e, n, a) {
    ((this.tag = t),
      (this.key = n),
      (this.sibling =
        this.child =
        this.return =
        this.stateNode =
        this.type =
        this.elementType =
          null),
      (this.index = 0),
      (this.refCleanup = this.ref = null),
      (this.pendingProps = e),
      (this.dependencies =
        this.memoizedState =
        this.updateQueue =
        this.memoizedProps =
          null),
      (this.mode = a),
      (this.subtreeFlags = this.flags = 0),
      (this.deletions = null),
      (this.childLanes = this.lanes = 0),
      (this.alternate = null));
  }
  function Ue(t, e, n, a) {
    return new Vp(t, e, n, a);
  }
  function Ls(t) {
    return ((t = t.prototype), !(!t || !t.isReactComponent));
  }
  function zn(t, e) {
    var n = t.alternate;
    return (
      n === null
        ? ((n = Ue(t.tag, e, t.key, t.mode)),
          (n.elementType = t.elementType),
          (n.type = t.type),
          (n.stateNode = t.stateNode),
          (n.alternate = t),
          (t.alternate = n))
        : ((n.pendingProps = e),
          (n.type = t.type),
          (n.flags = 0),
          (n.subtreeFlags = 0),
          (n.deletions = null)),
      (n.flags = t.flags & 65011712),
      (n.childLanes = t.childLanes),
      (n.lanes = t.lanes),
      (n.child = t.child),
      (n.memoizedProps = t.memoizedProps),
      (n.memoizedState = t.memoizedState),
      (n.updateQueue = t.updateQueue),
      (e = t.dependencies),
      (n.dependencies =
        e === null ? null : { lanes: e.lanes, firstContext: e.firstContext }),
      (n.sibling = t.sibling),
      (n.index = t.index),
      (n.ref = t.ref),
      (n.refCleanup = t.refCleanup),
      n
    );
  }
  function hd(t, e) {
    t.flags &= 65011714;
    var n = t.alternate;
    return (
      n === null
        ? ((t.childLanes = 0),
          (t.lanes = e),
          (t.child = null),
          (t.subtreeFlags = 0),
          (t.memoizedProps = null),
          (t.memoizedState = null),
          (t.updateQueue = null),
          (t.dependencies = null),
          (t.stateNode = null))
        : ((t.childLanes = n.childLanes),
          (t.lanes = n.lanes),
          (t.child = n.child),
          (t.subtreeFlags = 0),
          (t.deletions = null),
          (t.memoizedProps = n.memoizedProps),
          (t.memoizedState = n.memoizedState),
          (t.updateQueue = n.updateQueue),
          (t.type = n.type),
          (e = n.dependencies),
          (t.dependencies =
            e === null
              ? null
              : { lanes: e.lanes, firstContext: e.firstContext })),
      t
    );
  }
  function Xi(t, e, n, a, c, r) {
    var h = 0;
    if (((a = t), typeof t == "function")) Ls(t) && (h = 1);
    else if (typeof t == "string")
      h = kv(t, n, F.current)
        ? 26
        : t === "html" || t === "head" || t === "body"
          ? 27
          : 5;
    else
      t: switch (t) {
        case et:
          return (
            (t = Ue(31, n, e, c)),
            (t.elementType = et),
            (t.lanes = r),
            t
          );
        case z:
          return Vl(n.children, c, r, e);
        case B:
          ((h = 8), (c |= 24));
          break;
        case N:
          return (
            (t = Ue(12, n, e, c | 2)),
            (t.elementType = N),
            (t.lanes = r),
            t
          );
        case W:
          return ((t = Ue(13, n, e, c)), (t.elementType = W), (t.lanes = r), t);
        case I:
          return ((t = Ue(19, n, e, c)), (t.elementType = I), (t.lanes = r), t);
        default:
          if (typeof t == "object" && t !== null)
            switch (t.$$typeof) {
              case G:
                h = 10;
                break t;
              case Y:
                h = 9;
                break t;
              case j:
                h = 11;
                break t;
              case Q:
                h = 14;
                break t;
              case ut:
                ((h = 16), (a = null));
                break t;
            }
          ((h = 29),
            (n = Error(s(130, t === null ? "null" : typeof t, ""))),
            (a = null));
      }
    return (
      (e = Ue(h, n, e, c)),
      (e.elementType = t),
      (e.type = a),
      (e.lanes = r),
      e
    );
  }
  function Vl(t, e, n, a) {
    return ((t = Ue(7, t, a, e)), (t.lanes = n), t);
  }
  function Gs(t, e, n) {
    return ((t = Ue(6, t, null, e)), (t.lanes = n), t);
  }
  function md(t) {
    var e = Ue(18, null, null, 0);
    return ((e.stateNode = t), e);
  }
  function Zs(t, e, n) {
    return (
      (e = Ue(4, t.children !== null ? t.children : [], t.key, e)),
      (e.lanes = n),
      (e.stateNode = {
        containerInfo: t.containerInfo,
        pendingChildren: null,
        implementation: t.implementation,
      }),
      e
    );
  }
  var gd = new WeakMap();
  function Ke(t, e) {
    if (typeof t == "object" && t !== null) {
      var n = gd.get(t);
      return n !== void 0
        ? n
        : ((e = { value: t, source: e, stack: gt(e) }), gd.set(t, e), e);
    }
    return { value: t, source: e, stack: gt(e) };
  }
  var ya = [],
    pa = 0,
    Vi = null,
    gu = 0,
    $e = [],
    Je = 0,
    al = null,
    gn = 1,
    yn = "";
  function Mn(t, e) {
    ((ya[pa++] = gu), (ya[pa++] = Vi), (Vi = t), (gu = e));
  }
  function yd(t, e, n) {
    (($e[Je++] = gn), ($e[Je++] = yn), ($e[Je++] = al), (al = t));
    var a = gn;
    t = yn;
    var c = 32 - $t(a) - 1;
    ((a &= ~(1 << c)), (n += 1));
    var r = 32 - $t(e) + c;
    if (30 < r) {
      var h = c - (c % 5);
      ((r = (a & ((1 << h) - 1)).toString(32)),
        (a >>= h),
        (c -= h),
        (gn = (1 << (32 - $t(e) + c)) | (n << c) | a),
        (yn = r + t));
    } else ((gn = (1 << r) | (n << c) | a), (yn = t));
  }
  function Qs(t) {
    t.return !== null && (Mn(t, 1), yd(t, 1, 0));
  }
  function ks(t) {
    for (; t === Vi; )
      ((Vi = ya[--pa]), (ya[pa] = null), (gu = ya[--pa]), (ya[pa] = null));
    for (; t === al; )
      ((al = $e[--Je]),
        ($e[Je] = null),
        (yn = $e[--Je]),
        ($e[Je] = null),
        (gn = $e[--Je]),
        ($e[Je] = null));
  }
  function pd(t, e) {
    (($e[Je++] = gn),
      ($e[Je++] = yn),
      ($e[Je++] = al),
      (gn = e.id),
      (yn = e.overflow),
      (al = t));
  }
  var ye = null,
    Zt = null,
    Ct = !1,
    ul = null,
    We = !1,
    Ks = Error(s(519));
  function il(t) {
    var e = Error(
      s(
        418,
        1 < arguments.length && arguments[1] !== void 0 && arguments[1]
          ? "text"
          : "HTML",
        "",
      ),
    );
    throw (yu(Ke(e, t)), Ks);
  }
  function vd(t) {
    var e = t.stateNode,
      n = t.type,
      a = t.memoizedProps;
    switch (((e[re] = t), (e[ge] = a), n)) {
      case "dialog":
        (Tt("cancel", e), Tt("close", e));
        break;
      case "iframe":
      case "object":
      case "embed":
        Tt("load", e);
        break;
      case "video":
      case "audio":
        for (n = 0; n < Yu.length; n++) Tt(Yu[n], e);
        break;
      case "source":
        Tt("error", e);
        break;
      case "img":
      case "image":
      case "link":
        (Tt("error", e), Tt("load", e));
        break;
      case "details":
        Tt("toggle", e);
        break;
      case "input":
        (Tt("invalid", e),
          Df(
            e,
            a.value,
            a.defaultValue,
            a.checked,
            a.defaultChecked,
            a.type,
            a.name,
            !0,
          ));
        break;
      case "select":
        Tt("invalid", e);
        break;
      case "textarea":
        (Tt("invalid", e), Rf(e, a.value, a.defaultValue, a.children));
    }
    ((n = a.children),
      (typeof n != "string" && typeof n != "number" && typeof n != "bigint") ||
      e.textContent === "" + n ||
      a.suppressHydrationWarning === !0 ||
      jm(e.textContent, n)
        ? (a.popover != null && (Tt("beforetoggle", e), Tt("toggle", e)),
          a.onScroll != null && Tt("scroll", e),
          a.onScrollEnd != null && Tt("scrollend", e),
          a.onClick != null && (e.onclick = An),
          (e = !0))
        : (e = !1),
      e || il(t, !0));
  }
  function xd(t) {
    for (ye = t.return; ye; )
      switch (ye.tag) {
        case 5:
        case 31:
        case 13:
          We = !1;
          return;
        case 27:
        case 3:
          We = !0;
          return;
        default:
          ye = ye.return;
      }
  }
  function va(t) {
    if (t !== ye) return !1;
    if (!Ct) return (xd(t), (Ct = !0), !1);
    var e = t.tag,
      n;
    if (
      ((n = e !== 3 && e !== 27) &&
        ((n = e === 5) &&
          ((n = t.type),
          (n =
            !(n !== "form" && n !== "button") || fo(t.type, t.memoizedProps))),
        (n = !n)),
      n && Zt && il(t),
      xd(t),
      e === 13)
    ) {
      if (((t = t.memoizedState), (t = t !== null ? t.dehydrated : null), !t))
        throw Error(s(317));
      Zt = Zm(t);
    } else if (e === 31) {
      if (((t = t.memoizedState), (t = t !== null ? t.dehydrated : null), !t))
        throw Error(s(317));
      Zt = Zm(t);
    } else
      e === 27
        ? ((e = Zt), bl(t.type) ? ((t = po), (po = null), (Zt = t)) : (Zt = e))
        : (Zt = ye ? Ie(t.stateNode.nextSibling) : null);
    return !0;
  }
  function Ll() {
    ((Zt = ye = null), (Ct = !1));
  }
  function $s() {
    var t = ul;
    return (
      t !== null &&
        (Me === null ? (Me = t) : Me.push.apply(Me, t), (ul = null)),
      t
    );
  }
  function yu(t) {
    ul === null ? (ul = [t]) : ul.push(t);
  }
  var Js = b(null),
    Gl = null,
    Cn = null;
  function cl(t, e, n) {
    ($(Js, e._currentValue), (e._currentValue = n));
  }
  function Dn(t) {
    ((t._currentValue = Js.current), M(Js));
  }
  function Ws(t, e, n) {
    for (; t !== null; ) {
      var a = t.alternate;
      if (
        ((t.childLanes & e) !== e
          ? ((t.childLanes |= e), a !== null && (a.childLanes |= e))
          : a !== null && (a.childLanes & e) !== e && (a.childLanes |= e),
        t === n)
      )
        break;
      t = t.return;
    }
  }
  function Fs(t, e, n, a) {
    var c = t.child;
    for (c !== null && (c.return = t); c !== null; ) {
      var r = c.dependencies;
      if (r !== null) {
        var h = c.child;
        r = r.firstContext;
        t: for (; r !== null; ) {
          var v = r;
          r = c;
          for (var T = 0; T < e.length; T++)
            if (v.context === e[T]) {
              ((r.lanes |= n),
                (v = r.alternate),
                v !== null && (v.lanes |= n),
                Ws(r.return, n, t),
                a || (h = null));
              break t;
            }
          r = v.next;
        }
      } else if (c.tag === 18) {
        if (((h = c.return), h === null)) throw Error(s(341));
        ((h.lanes |= n),
          (r = h.alternate),
          r !== null && (r.lanes |= n),
          Ws(h, n, t),
          (h = null));
      } else h = c.child;
      if (h !== null) h.return = c;
      else
        for (h = c; h !== null; ) {
          if (h === t) {
            h = null;
            break;
          }
          if (((c = h.sibling), c !== null)) {
            ((c.return = h.return), (h = c));
            break;
          }
          h = h.return;
        }
      c = h;
    }
  }
  function xa(t, e, n, a) {
    t = null;
    for (var c = e, r = !1; c !== null; ) {
      if (!r) {
        if ((c.flags & 524288) !== 0) r = !0;
        else if ((c.flags & 262144) !== 0) break;
      }
      if (c.tag === 10) {
        var h = c.alternate;
        if (h === null) throw Error(s(387));
        if (((h = h.memoizedProps), h !== null)) {
          var v = c.type;
          je(c.pendingProps.value, h.value) ||
            (t !== null ? t.push(v) : (t = [v]));
        }
      } else if (c === dt.current) {
        if (((h = c.alternate), h === null)) throw Error(s(387));
        h.memoizedState.memoizedState !== c.memoizedState.memoizedState &&
          (t !== null ? t.push(Gu) : (t = [Gu]));
      }
      c = c.return;
    }
    (t !== null && Fs(e, t, n, a), (e.flags |= 262144));
  }
  function Li(t) {
    for (t = t.firstContext; t !== null; ) {
      if (!je(t.context._currentValue, t.memoizedValue)) return !0;
      t = t.next;
    }
    return !1;
  }
  function Zl(t) {
    ((Gl = t),
      (Cn = null),
      (t = t.dependencies),
      t !== null && (t.firstContext = null));
  }
  function pe(t) {
    return bd(Gl, t);
  }
  function Gi(t, e) {
    return (Gl === null && Zl(t), bd(t, e));
  }
  function bd(t, e) {
    var n = e._currentValue;
    if (((e = { context: e, memoizedValue: n, next: null }), Cn === null)) {
      if (t === null) throw Error(s(308));
      ((Cn = e),
        (t.dependencies = { lanes: 0, firstContext: e }),
        (t.flags |= 524288));
    } else Cn = Cn.next = e;
    return n;
  }
  var Lp =
      typeof AbortController < "u"
        ? AbortController
        : function () {
            var t = [],
              e = (this.signal = {
                aborted: !1,
                addEventListener: function (n, a) {
                  t.push(a);
                },
              });
            this.abort = function () {
              ((e.aborted = !0),
                t.forEach(function (n) {
                  return n();
                }));
            };
          },
    Gp = l.unstable_scheduleCallback,
    Zp = l.unstable_NormalPriority,
    ae = {
      $$typeof: G,
      Consumer: null,
      Provider: null,
      _currentValue: null,
      _currentValue2: null,
      _threadCount: 0,
    };
  function Is() {
    return { controller: new Lp(), data: new Map(), refCount: 0 };
  }
  function pu(t) {
    (t.refCount--,
      t.refCount === 0 &&
        Gp(Zp, function () {
          t.controller.abort();
        }));
  }
  var vu = null,
    Ps = 0,
    ba = 0,
    Sa = null;
  function Qp(t, e) {
    if (vu === null) {
      var n = (vu = []);
      ((Ps = 0),
        (ba = no()),
        (Sa = {
          status: "pending",
          value: void 0,
          then: function (a) {
            n.push(a);
          },
        }));
    }
    return (Ps++, e.then(Sd, Sd), e);
  }
  function Sd() {
    if (--Ps === 0 && vu !== null) {
      Sa !== null && (Sa.status = "fulfilled");
      var t = vu;
      ((vu = null), (ba = 0), (Sa = null));
      for (var e = 0; e < t.length; e++) (0, t[e])();
    }
  }
  function kp(t, e) {
    var n = [],
      a = {
        status: "pending",
        value: null,
        reason: null,
        then: function (c) {
          n.push(c);
        },
      };
    return (
      t.then(
        function () {
          ((a.status = "fulfilled"), (a.value = e));
          for (var c = 0; c < n.length; c++) (0, n[c])(e);
        },
        function (c) {
          for (a.status = "rejected", a.reason = c, c = 0; c < n.length; c++)
            (0, n[c])(void 0);
        },
      ),
      a
    );
  }
  var Ed = S.S;
  S.S = function (t, e) {
    ((um = Kt()),
      typeof e == "object" &&
        e !== null &&
        typeof e.then == "function" &&
        Qp(t, e),
      Ed !== null && Ed(t, e));
  };
  var Ql = b(null);
  function tr() {
    var t = Ql.current;
    return t !== null ? t : Xt.pooledCache;
  }
  function Zi(t, e) {
    e === null ? $(Ql, Ql.current) : $(Ql, e.pool);
  }
  function _d() {
    var t = tr();
    return t === null ? null : { parent: ae._currentValue, pool: t };
  }
  var Ea = Error(s(460)),
    er = Error(s(474)),
    Qi = Error(s(542)),
    ki = { then: function () {} };
  function wd(t) {
    return ((t = t.status), t === "fulfilled" || t === "rejected");
  }
  function Nd(t, e, n) {
    switch (
      ((n = t[n]),
      n === void 0 ? t.push(e) : n !== e && (e.then(An, An), (e = n)),
      e.status)
    ) {
      case "fulfilled":
        return e.value;
      case "rejected":
        throw ((t = e.reason), Td(t), t);
      default:
        if (typeof e.status == "string") e.then(An, An);
        else {
          if (((t = Xt), t !== null && 100 < t.shellSuspendCounter))
            throw Error(s(482));
          ((t = e),
            (t.status = "pending"),
            t.then(
              function (a) {
                if (e.status === "pending") {
                  var c = e;
                  ((c.status = "fulfilled"), (c.value = a));
                }
              },
              function (a) {
                if (e.status === "pending") {
                  var c = e;
                  ((c.status = "rejected"), (c.reason = a));
                }
              },
            ));
        }
        switch (e.status) {
          case "fulfilled":
            return e.value;
          case "rejected":
            throw ((t = e.reason), Td(t), t);
        }
        throw ((Kl = e), Ea);
    }
  }
  function kl(t) {
    try {
      var e = t._init;
      return e(t._payload);
    } catch (n) {
      throw n !== null && typeof n == "object" && typeof n.then == "function"
        ? ((Kl = n), Ea)
        : n;
    }
  }
  var Kl = null;
  function Ad() {
    if (Kl === null) throw Error(s(459));
    var t = Kl;
    return ((Kl = null), t);
  }
  function Td(t) {
    if (t === Ea || t === Qi) throw Error(s(483));
  }
  var _a = null,
    xu = 0;
  function Ki(t) {
    var e = xu;
    return ((xu += 1), _a === null && (_a = []), Nd(_a, t, e));
  }
  function bu(t, e) {
    ((e = e.props.ref), (t.ref = e !== void 0 ? e : null));
  }
  function $i(t, e) {
    throw e.$$typeof === _
      ? Error(s(525))
      : ((t = Object.prototype.toString.call(e)),
        Error(
          s(
            31,
            t === "[object Object]"
              ? "object with keys {" + Object.keys(e).join(", ") + "}"
              : t,
          ),
        ));
  }
  function zd(t) {
    function e(U, C) {
      if (t) {
        var q = U.deletions;
        q === null ? ((U.deletions = [C]), (U.flags |= 16)) : q.push(C);
      }
    }
    function n(U, C) {
      if (!t) return null;
      for (; C !== null; ) (e(U, C), (C = C.sibling));
      return null;
    }
    function a(U) {
      for (var C = new Map(); U !== null; )
        (U.key !== null ? C.set(U.key, U) : C.set(U.index, U), (U = U.sibling));
      return C;
    }
    function c(U, C) {
      return ((U = zn(U, C)), (U.index = 0), (U.sibling = null), U);
    }
    function r(U, C, q) {
      return (
        (U.index = q),
        t
          ? ((q = U.alternate),
            q !== null
              ? ((q = q.index), q < C ? ((U.flags |= 67108866), C) : q)
              : ((U.flags |= 67108866), C))
          : ((U.flags |= 1048576), C)
      );
    }
    function h(U) {
      return (t && U.alternate === null && (U.flags |= 67108866), U);
    }
    function v(U, C, q, nt) {
      return C === null || C.tag !== 6
        ? ((C = Gs(q, U.mode, nt)), (C.return = U), C)
        : ((C = c(C, q)), (C.return = U), C);
    }
    function T(U, C, q, nt) {
      var vt = q.type;
      return vt === z
        ? P(U, C, q.props.children, nt, q.key)
        : C !== null &&
            (C.elementType === vt ||
              (typeof vt == "object" &&
                vt !== null &&
                vt.$$typeof === ut &&
                kl(vt) === C.type))
          ? ((C = c(C, q.props)), bu(C, q), (C.return = U), C)
          : ((C = Xi(q.type, q.key, q.props, null, U.mode, nt)),
            bu(C, q),
            (C.return = U),
            C);
    }
    function X(U, C, q, nt) {
      return C === null ||
        C.tag !== 4 ||
        C.stateNode.containerInfo !== q.containerInfo ||
        C.stateNode.implementation !== q.implementation
        ? ((C = Zs(q, U.mode, nt)), (C.return = U), C)
        : ((C = c(C, q.children || [])), (C.return = U), C);
    }
    function P(U, C, q, nt, vt) {
      return C === null || C.tag !== 7
        ? ((C = Vl(q, U.mode, nt, vt)), (C.return = U), C)
        : ((C = c(C, q)), (C.return = U), C);
    }
    function lt(U, C, q) {
      if (
        (typeof C == "string" && C !== "") ||
        typeof C == "number" ||
        typeof C == "bigint"
      )
        return ((C = Gs("" + C, U.mode, q)), (C.return = U), C);
      if (typeof C == "object" && C !== null) {
        switch (C.$$typeof) {
          case E:
            return (
              (q = Xi(C.type, C.key, C.props, null, U.mode, q)),
              bu(q, C),
              (q.return = U),
              q
            );
          case D:
            return ((C = Zs(C, U.mode, q)), (C.return = U), C);
          case ut:
            return ((C = kl(C)), lt(U, C, q));
        }
        if (L(C) || st(C))
          return ((C = Vl(C, U.mode, q, null)), (C.return = U), C);
        if (typeof C.then == "function") return lt(U, Ki(C), q);
        if (C.$$typeof === G) return lt(U, Gi(U, C), q);
        $i(U, C);
      }
      return null;
    }
    function V(U, C, q, nt) {
      var vt = C !== null ? C.key : null;
      if (
        (typeof q == "string" && q !== "") ||
        typeof q == "number" ||
        typeof q == "bigint"
      )
        return vt !== null ? null : v(U, C, "" + q, nt);
      if (typeof q == "object" && q !== null) {
        switch (q.$$typeof) {
          case E:
            return q.key === vt ? T(U, C, q, nt) : null;
          case D:
            return q.key === vt ? X(U, C, q, nt) : null;
          case ut:
            return ((q = kl(q)), V(U, C, q, nt));
        }
        if (L(q) || st(q)) return vt !== null ? null : P(U, C, q, nt, null);
        if (typeof q.then == "function") return V(U, C, Ki(q), nt);
        if (q.$$typeof === G) return V(U, C, Gi(U, q), nt);
        $i(U, q);
      }
      return null;
    }
    function K(U, C, q, nt, vt) {
      if (
        (typeof nt == "string" && nt !== "") ||
        typeof nt == "number" ||
        typeof nt == "bigint"
      )
        return ((U = U.get(q) || null), v(C, U, "" + nt, vt));
      if (typeof nt == "object" && nt !== null) {
        switch (nt.$$typeof) {
          case E:
            return (
              (U = U.get(nt.key === null ? q : nt.key) || null),
              T(C, U, nt, vt)
            );
          case D:
            return (
              (U = U.get(nt.key === null ? q : nt.key) || null),
              X(C, U, nt, vt)
            );
          case ut:
            return ((nt = kl(nt)), K(U, C, q, nt, vt));
        }
        if (L(nt) || st(nt))
          return ((U = U.get(q) || null), P(C, U, nt, vt, null));
        if (typeof nt.then == "function") return K(U, C, q, Ki(nt), vt);
        if (nt.$$typeof === G) return K(U, C, q, Gi(C, nt), vt);
        $i(C, nt);
      }
      return null;
    }
    function mt(U, C, q, nt) {
      for (
        var vt = null, Ot = null, yt = C, _t = (C = 0), Mt = null;
        yt !== null && _t < q.length;
        _t++
      ) {
        yt.index > _t ? ((Mt = yt), (yt = null)) : (Mt = yt.sibling);
        var Rt = V(U, yt, q[_t], nt);
        if (Rt === null) {
          yt === null && (yt = Mt);
          break;
        }
        (t && yt && Rt.alternate === null && e(U, yt),
          (C = r(Rt, C, _t)),
          Ot === null ? (vt = Rt) : (Ot.sibling = Rt),
          (Ot = Rt),
          (yt = Mt));
      }
      if (_t === q.length) return (n(U, yt), Ct && Mn(U, _t), vt);
      if (yt === null) {
        for (; _t < q.length; _t++)
          ((yt = lt(U, q[_t], nt)),
            yt !== null &&
              ((C = r(yt, C, _t)),
              Ot === null ? (vt = yt) : (Ot.sibling = yt),
              (Ot = yt)));
        return (Ct && Mn(U, _t), vt);
      }
      for (yt = a(yt); _t < q.length; _t++)
        ((Mt = K(yt, U, _t, q[_t], nt)),
          Mt !== null &&
            (t &&
              Mt.alternate !== null &&
              yt.delete(Mt.key === null ? _t : Mt.key),
            (C = r(Mt, C, _t)),
            Ot === null ? (vt = Mt) : (Ot.sibling = Mt),
            (Ot = Mt)));
      return (
        t &&
          yt.forEach(function (Nl) {
            return e(U, Nl);
          }),
        Ct && Mn(U, _t),
        vt
      );
    }
    function xt(U, C, q, nt) {
      if (q == null) throw Error(s(151));
      for (
        var vt = null,
          Ot = null,
          yt = C,
          _t = (C = 0),
          Mt = null,
          Rt = q.next();
        yt !== null && !Rt.done;
        _t++, Rt = q.next()
      ) {
        yt.index > _t ? ((Mt = yt), (yt = null)) : (Mt = yt.sibling);
        var Nl = V(U, yt, Rt.value, nt);
        if (Nl === null) {
          yt === null && (yt = Mt);
          break;
        }
        (t && yt && Nl.alternate === null && e(U, yt),
          (C = r(Nl, C, _t)),
          Ot === null ? (vt = Nl) : (Ot.sibling = Nl),
          (Ot = Nl),
          (yt = Mt));
      }
      if (Rt.done) return (n(U, yt), Ct && Mn(U, _t), vt);
      if (yt === null) {
        for (; !Rt.done; _t++, Rt = q.next())
          ((Rt = lt(U, Rt.value, nt)),
            Rt !== null &&
              ((C = r(Rt, C, _t)),
              Ot === null ? (vt = Rt) : (Ot.sibling = Rt),
              (Ot = Rt)));
        return (Ct && Mn(U, _t), vt);
      }
      for (yt = a(yt); !Rt.done; _t++, Rt = q.next())
        ((Rt = K(yt, U, _t, Rt.value, nt)),
          Rt !== null &&
            (t &&
              Rt.alternate !== null &&
              yt.delete(Rt.key === null ? _t : Rt.key),
            (C = r(Rt, C, _t)),
            Ot === null ? (vt = Rt) : (Ot.sibling = Rt),
            (Ot = Rt)));
      return (
        t &&
          yt.forEach(function (l1) {
            return e(U, l1);
          }),
        Ct && Mn(U, _t),
        vt
      );
    }
    function qt(U, C, q, nt) {
      if (
        (typeof q == "object" &&
          q !== null &&
          q.type === z &&
          q.key === null &&
          (q = q.props.children),
        typeof q == "object" && q !== null)
      ) {
        switch (q.$$typeof) {
          case E:
            t: {
              for (var vt = q.key; C !== null; ) {
                if (C.key === vt) {
                  if (((vt = q.type), vt === z)) {
                    if (C.tag === 7) {
                      (n(U, C.sibling),
                        (nt = c(C, q.props.children)),
                        (nt.return = U),
                        (U = nt));
                      break t;
                    }
                  } else if (
                    C.elementType === vt ||
                    (typeof vt == "object" &&
                      vt !== null &&
                      vt.$$typeof === ut &&
                      kl(vt) === C.type)
                  ) {
                    (n(U, C.sibling),
                      (nt = c(C, q.props)),
                      bu(nt, q),
                      (nt.return = U),
                      (U = nt));
                    break t;
                  }
                  n(U, C);
                  break;
                } else e(U, C);
                C = C.sibling;
              }
              q.type === z
                ? ((nt = Vl(q.props.children, U.mode, nt, q.key)),
                  (nt.return = U),
                  (U = nt))
                : ((nt = Xi(q.type, q.key, q.props, null, U.mode, nt)),
                  bu(nt, q),
                  (nt.return = U),
                  (U = nt));
            }
            return h(U);
          case D:
            t: {
              for (vt = q.key; C !== null; ) {
                if (C.key === vt)
                  if (
                    C.tag === 4 &&
                    C.stateNode.containerInfo === q.containerInfo &&
                    C.stateNode.implementation === q.implementation
                  ) {
                    (n(U, C.sibling),
                      (nt = c(C, q.children || [])),
                      (nt.return = U),
                      (U = nt));
                    break t;
                  } else {
                    n(U, C);
                    break;
                  }
                else e(U, C);
                C = C.sibling;
              }
              ((nt = Zs(q, U.mode, nt)), (nt.return = U), (U = nt));
            }
            return h(U);
          case ut:
            return ((q = kl(q)), qt(U, C, q, nt));
        }
        if (L(q)) return mt(U, C, q, nt);
        if (st(q)) {
          if (((vt = st(q)), typeof vt != "function")) throw Error(s(150));
          return ((q = vt.call(q)), xt(U, C, q, nt));
        }
        if (typeof q.then == "function") return qt(U, C, Ki(q), nt);
        if (q.$$typeof === G) return qt(U, C, Gi(U, q), nt);
        $i(U, q);
      }
      return (typeof q == "string" && q !== "") ||
        typeof q == "number" ||
        typeof q == "bigint"
        ? ((q = "" + q),
          C !== null && C.tag === 6
            ? (n(U, C.sibling), (nt = c(C, q)), (nt.return = U), (U = nt))
            : (n(U, C), (nt = Gs(q, U.mode, nt)), (nt.return = U), (U = nt)),
          h(U))
        : n(U, C);
    }
    return function (U, C, q, nt) {
      try {
        xu = 0;
        var vt = qt(U, C, q, nt);
        return ((_a = null), vt);
      } catch (yt) {
        if (yt === Ea || yt === Qi) throw yt;
        var Ot = Ue(29, yt, null, U.mode);
        return ((Ot.lanes = nt), (Ot.return = U), Ot);
      } finally {
      }
    };
  }
  var $l = zd(!0),
    Md = zd(!1),
    sl = !1;
  function nr(t) {
    t.updateQueue = {
      baseState: t.memoizedState,
      firstBaseUpdate: null,
      lastBaseUpdate: null,
      shared: { pending: null, lanes: 0, hiddenCallbacks: null },
      callbacks: null,
    };
  }
  function lr(t, e) {
    ((t = t.updateQueue),
      e.updateQueue === t &&
        (e.updateQueue = {
          baseState: t.baseState,
          firstBaseUpdate: t.firstBaseUpdate,
          lastBaseUpdate: t.lastBaseUpdate,
          shared: t.shared,
          callbacks: null,
        }));
  }
  function rl(t) {
    return { lane: t, tag: 0, payload: null, callback: null, next: null };
  }
  function ol(t, e, n) {
    var a = t.updateQueue;
    if (a === null) return null;
    if (((a = a.shared), (Ht & 2) !== 0)) {
      var c = a.pending;
      return (
        c === null ? (e.next = e) : ((e.next = c.next), (c.next = e)),
        (a.pending = e),
        (e = qi(t)),
        dd(t, null, n),
        e
      );
    }
    return (Yi(t, a, e, n), qi(t));
  }
  function Su(t, e, n) {
    if (
      ((e = e.updateQueue), e !== null && ((e = e.shared), (n & 4194048) !== 0))
    ) {
      var a = e.lanes;
      ((a &= t.pendingLanes), (n |= a), (e.lanes = n), pi(t, n));
    }
  }
  function ar(t, e) {
    var n = t.updateQueue,
      a = t.alternate;
    if (a !== null && ((a = a.updateQueue), n === a)) {
      var c = null,
        r = null;
      if (((n = n.firstBaseUpdate), n !== null)) {
        do {
          var h = {
            lane: n.lane,
            tag: n.tag,
            payload: n.payload,
            callback: null,
            next: null,
          };
          (r === null ? (c = r = h) : (r = r.next = h), (n = n.next));
        } while (n !== null);
        r === null ? (c = r = e) : (r = r.next = e);
      } else c = r = e;
      ((n = {
        baseState: a.baseState,
        firstBaseUpdate: c,
        lastBaseUpdate: r,
        shared: a.shared,
        callbacks: a.callbacks,
      }),
        (t.updateQueue = n));
      return;
    }
    ((t = n.lastBaseUpdate),
      t === null ? (n.firstBaseUpdate = e) : (t.next = e),
      (n.lastBaseUpdate = e));
  }
  var ur = !1;
  function Eu() {
    if (ur) {
      var t = Sa;
      if (t !== null) throw t;
    }
  }
  function _u(t, e, n, a) {
    ur = !1;
    var c = t.updateQueue;
    sl = !1;
    var r = c.firstBaseUpdate,
      h = c.lastBaseUpdate,
      v = c.shared.pending;
    if (v !== null) {
      c.shared.pending = null;
      var T = v,
        X = T.next;
      ((T.next = null), h === null ? (r = X) : (h.next = X), (h = T));
      var P = t.alternate;
      P !== null &&
        ((P = P.updateQueue),
        (v = P.lastBaseUpdate),
        v !== h &&
          (v === null ? (P.firstBaseUpdate = X) : (v.next = X),
          (P.lastBaseUpdate = T)));
    }
    if (r !== null) {
      var lt = c.baseState;
      ((h = 0), (P = X = T = null), (v = r));
      do {
        var V = v.lane & -536870913,
          K = V !== v.lane;
        if (K ? (zt & V) === V : (a & V) === V) {
          (V !== 0 && V === ba && (ur = !0),
            P !== null &&
              (P = P.next =
                {
                  lane: 0,
                  tag: v.tag,
                  payload: v.payload,
                  callback: null,
                  next: null,
                }));
          t: {
            var mt = t,
              xt = v;
            V = e;
            var qt = n;
            switch (xt.tag) {
              case 1:
                if (((mt = xt.payload), typeof mt == "function")) {
                  lt = mt.call(qt, lt, V);
                  break t;
                }
                lt = mt;
                break t;
              case 3:
                mt.flags = (mt.flags & -65537) | 128;
              case 0:
                if (
                  ((mt = xt.payload),
                  (V = typeof mt == "function" ? mt.call(qt, lt, V) : mt),
                  V == null)
                )
                  break t;
                lt = x({}, lt, V);
                break t;
              case 2:
                sl = !0;
            }
          }
          ((V = v.callback),
            V !== null &&
              ((t.flags |= 64),
              K && (t.flags |= 8192),
              (K = c.callbacks),
              K === null ? (c.callbacks = [V]) : K.push(V)));
        } else
          ((K = {
            lane: V,
            tag: v.tag,
            payload: v.payload,
            callback: v.callback,
            next: null,
          }),
            P === null ? ((X = P = K), (T = lt)) : (P = P.next = K),
            (h |= V));
        if (((v = v.next), v === null)) {
          if (((v = c.shared.pending), v === null)) break;
          ((K = v),
            (v = K.next),
            (K.next = null),
            (c.lastBaseUpdate = K),
            (c.shared.pending = null));
        }
      } while (!0);
      (P === null && (T = lt),
        (c.baseState = T),
        (c.firstBaseUpdate = X),
        (c.lastBaseUpdate = P),
        r === null && (c.shared.lanes = 0),
        (gl |= h),
        (t.lanes = h),
        (t.memoizedState = lt));
    }
  }
  function Cd(t, e) {
    if (typeof t != "function") throw Error(s(191, t));
    t.call(e);
  }
  function Dd(t, e) {
    var n = t.callbacks;
    if (n !== null)
      for (t.callbacks = null, t = 0; t < n.length; t++) Cd(n[t], e);
  }
  var wa = b(null),
    Ji = b(0);
  function Od(t, e) {
    ((t = Xn), $(Ji, t), $(wa, e), (Xn = t | e.baseLanes));
  }
  function ir() {
    ($(Ji, Xn), $(wa, wa.current));
  }
  function cr() {
    ((Xn = Ji.current), M(wa), M(Ji));
  }
  var Be = b(null),
    Fe = null;
  function fl(t) {
    var e = t.alternate;
    ($(te, te.current & 1),
      $(Be, t),
      Fe === null &&
        (e === null || wa.current !== null || e.memoizedState !== null) &&
        (Fe = t));
  }
  function sr(t) {
    ($(te, te.current), $(Be, t), Fe === null && (Fe = t));
  }
  function Rd(t) {
    t.tag === 22
      ? ($(te, te.current), $(Be, t), Fe === null && (Fe = t))
      : dl();
  }
  function dl() {
    ($(te, te.current), $(Be, Be.current));
  }
  function Ye(t) {
    (M(Be), Fe === t && (Fe = null), M(te));
  }
  var te = b(0);
  function Wi(t) {
    for (var e = t; e !== null; ) {
      if (e.tag === 13) {
        var n = e.memoizedState;
        if (n !== null && ((n = n.dehydrated), n === null || go(n) || yo(n)))
          return e;
      } else if (
        e.tag === 19 &&
        (e.memoizedProps.revealOrder === "forwards" ||
          e.memoizedProps.revealOrder === "backwards" ||
          e.memoizedProps.revealOrder === "unstable_legacy-backwards" ||
          e.memoizedProps.revealOrder === "together")
      ) {
        if ((e.flags & 128) !== 0) return e;
      } else if (e.child !== null) {
        ((e.child.return = e), (e = e.child));
        continue;
      }
      if (e === t) break;
      for (; e.sibling === null; ) {
        if (e.return === null || e.return === t) return null;
        e = e.return;
      }
      ((e.sibling.return = e.return), (e = e.sibling));
    }
    return null;
  }
  var On = 0,
    Et = null,
    Bt = null,
    ue = null,
    Fi = !1,
    Na = !1,
    Jl = !1,
    Ii = 0,
    wu = 0,
    Aa = null,
    Kp = 0;
  function Jt() {
    throw Error(s(321));
  }
  function rr(t, e) {
    if (e === null) return !1;
    for (var n = 0; n < e.length && n < t.length; n++)
      if (!je(t[n], e[n])) return !1;
    return !0;
  }
  function or(t, e, n, a, c, r) {
    return (
      (On = r),
      (Et = e),
      (e.memoizedState = null),
      (e.updateQueue = null),
      (e.lanes = 0),
      (S.H = t === null || t.memoizedState === null ? yh : Nr),
      (Jl = !1),
      (r = n(a, c)),
      (Jl = !1),
      Na && (r = jd(e, n, a, c)),
      Hd(t),
      r
    );
  }
  function Hd(t) {
    S.H = Tu;
    var e = Bt !== null && Bt.next !== null;
    if (((On = 0), (ue = Bt = Et = null), (Fi = !1), (wu = 0), (Aa = null), e))
      throw Error(s(300));
    t === null ||
      ie ||
      ((t = t.dependencies), t !== null && Li(t) && (ie = !0));
  }
  function jd(t, e, n, a) {
    Et = t;
    var c = 0;
    do {
      if ((Na && (Aa = null), (wu = 0), (Na = !1), 25 <= c))
        throw Error(s(301));
      if (((c += 1), (ue = Bt = null), t.updateQueue != null)) {
        var r = t.updateQueue;
        ((r.lastEffect = null),
          (r.events = null),
          (r.stores = null),
          r.memoCache != null && (r.memoCache.index = 0));
      }
      ((S.H = ph), (r = e(n, a)));
    } while (Na);
    return r;
  }
  function $p() {
    var t = S.H,
      e = t.useState()[0];
    return (
      (e = typeof e.then == "function" ? Nu(e) : e),
      (t = t.useState()[0]),
      (Bt !== null ? Bt.memoizedState : null) !== t && (Et.flags |= 1024),
      e
    );
  }
  function fr() {
    var t = Ii !== 0;
    return ((Ii = 0), t);
  }
  function dr(t, e, n) {
    ((e.updateQueue = t.updateQueue), (e.flags &= -2053), (t.lanes &= ~n));
  }
  function hr(t) {
    if (Fi) {
      for (t = t.memoizedState; t !== null; ) {
        var e = t.queue;
        (e !== null && (e.pending = null), (t = t.next));
      }
      Fi = !1;
    }
    ((On = 0), (ue = Bt = Et = null), (Na = !1), (wu = Ii = 0), (Aa = null));
  }
  function we() {
    var t = {
      memoizedState: null,
      baseState: null,
      baseQueue: null,
      queue: null,
      next: null,
    };
    return (ue === null ? (Et.memoizedState = ue = t) : (ue = ue.next = t), ue);
  }
  function ee() {
    if (Bt === null) {
      var t = Et.alternate;
      t = t !== null ? t.memoizedState : null;
    } else t = Bt.next;
    var e = ue === null ? Et.memoizedState : ue.next;
    if (e !== null) ((ue = e), (Bt = t));
    else {
      if (t === null)
        throw Et.alternate === null ? Error(s(467)) : Error(s(310));
      ((Bt = t),
        (t = {
          memoizedState: Bt.memoizedState,
          baseState: Bt.baseState,
          baseQueue: Bt.baseQueue,
          queue: Bt.queue,
          next: null,
        }),
        ue === null ? (Et.memoizedState = ue = t) : (ue = ue.next = t));
    }
    return ue;
  }
  function Pi() {
    return { lastEffect: null, events: null, stores: null, memoCache: null };
  }
  function Nu(t) {
    var e = wu;
    return (
      (wu += 1),
      Aa === null && (Aa = []),
      (t = Nd(Aa, t, e)),
      (e = Et),
      (ue === null ? e.memoizedState : ue.next) === null &&
        ((e = e.alternate),
        (S.H = e === null || e.memoizedState === null ? yh : Nr)),
      t
    );
  }
  function tc(t) {
    if (t !== null && typeof t == "object") {
      if (typeof t.then == "function") return Nu(t);
      if (t.$$typeof === G) return pe(t);
    }
    throw Error(s(438, String(t)));
  }
  function mr(t) {
    var e = null,
      n = Et.updateQueue;
    if ((n !== null && (e = n.memoCache), e == null)) {
      var a = Et.alternate;
      a !== null &&
        ((a = a.updateQueue),
        a !== null &&
          ((a = a.memoCache),
          a != null &&
            (e = {
              data: a.data.map(function (c) {
                return c.slice();
              }),
              index: 0,
            })));
    }
    if (
      (e == null && (e = { data: [], index: 0 }),
      n === null && ((n = Pi()), (Et.updateQueue = n)),
      (n.memoCache = e),
      (n = e.data[e.index]),
      n === void 0)
    )
      for (n = e.data[e.index] = Array(t), a = 0; a < t; a++) n[a] = ct;
    return (e.index++, n);
  }
  function Rn(t, e) {
    return typeof e == "function" ? e(t) : e;
  }
  function ec(t) {
    var e = ee();
    return gr(e, Bt, t);
  }
  function gr(t, e, n) {
    var a = t.queue;
    if (a === null) throw Error(s(311));
    a.lastRenderedReducer = n;
    var c = t.baseQueue,
      r = a.pending;
    if (r !== null) {
      if (c !== null) {
        var h = c.next;
        ((c.next = r.next), (r.next = h));
      }
      ((e.baseQueue = c = r), (a.pending = null));
    }
    if (((r = t.baseState), c === null)) t.memoizedState = r;
    else {
      e = c.next;
      var v = (h = null),
        T = null,
        X = e,
        P = !1;
      do {
        var lt = X.lane & -536870913;
        if (lt !== X.lane ? (zt & lt) === lt : (On & lt) === lt) {
          var V = X.revertLane;
          if (V === 0)
            (T !== null &&
              (T = T.next =
                {
                  lane: 0,
                  revertLane: 0,
                  gesture: null,
                  action: X.action,
                  hasEagerState: X.hasEagerState,
                  eagerState: X.eagerState,
                  next: null,
                }),
              lt === ba && (P = !0));
          else if ((On & V) === V) {
            ((X = X.next), V === ba && (P = !0));
            continue;
          } else
            ((lt = {
              lane: 0,
              revertLane: X.revertLane,
              gesture: null,
              action: X.action,
              hasEagerState: X.hasEagerState,
              eagerState: X.eagerState,
              next: null,
            }),
              T === null ? ((v = T = lt), (h = r)) : (T = T.next = lt),
              (Et.lanes |= V),
              (gl |= V));
          ((lt = X.action),
            Jl && n(r, lt),
            (r = X.hasEagerState ? X.eagerState : n(r, lt)));
        } else
          ((V = {
            lane: lt,
            revertLane: X.revertLane,
            gesture: X.gesture,
            action: X.action,
            hasEagerState: X.hasEagerState,
            eagerState: X.eagerState,
            next: null,
          }),
            T === null ? ((v = T = V), (h = r)) : (T = T.next = V),
            (Et.lanes |= lt),
            (gl |= lt));
        X = X.next;
      } while (X !== null && X !== e);
      if (
        (T === null ? (h = r) : (T.next = v),
        !je(r, t.memoizedState) && ((ie = !0), P && ((n = Sa), n !== null)))
      )
        throw n;
      ((t.memoizedState = r),
        (t.baseState = h),
        (t.baseQueue = T),
        (a.lastRenderedState = r));
    }
    return (c === null && (a.lanes = 0), [t.memoizedState, a.dispatch]);
  }
  function yr(t) {
    var e = ee(),
      n = e.queue;
    if (n === null) throw Error(s(311));
    n.lastRenderedReducer = t;
    var a = n.dispatch,
      c = n.pending,
      r = e.memoizedState;
    if (c !== null) {
      n.pending = null;
      var h = (c = c.next);
      do ((r = t(r, h.action)), (h = h.next));
      while (h !== c);
      (je(r, e.memoizedState) || (ie = !0),
        (e.memoizedState = r),
        e.baseQueue === null && (e.baseState = r),
        (n.lastRenderedState = r));
    }
    return [r, a];
  }
  function Ud(t, e, n) {
    var a = Et,
      c = ee(),
      r = Ct;
    if (r) {
      if (n === void 0) throw Error(s(407));
      n = n();
    } else n = e();
    var h = !je((Bt || c).memoizedState, n);
    if (
      (h && ((c.memoizedState = n), (ie = !0)),
      (c = c.queue),
      xr(qd.bind(null, a, c, t), [t]),
      c.getSnapshot !== e || h || (ue !== null && ue.memoizedState.tag & 1))
    ) {
      if (
        ((a.flags |= 2048),
        Ta(9, { destroy: void 0 }, Yd.bind(null, a, c, n, e), null),
        Xt === null)
      )
        throw Error(s(349));
      r || (On & 127) !== 0 || Bd(a, e, n);
    }
    return n;
  }
  function Bd(t, e, n) {
    ((t.flags |= 16384),
      (t = { getSnapshot: e, value: n }),
      (e = Et.updateQueue),
      e === null
        ? ((e = Pi()), (Et.updateQueue = e), (e.stores = [t]))
        : ((n = e.stores), n === null ? (e.stores = [t]) : n.push(t)));
  }
  function Yd(t, e, n, a) {
    ((e.value = n), (e.getSnapshot = a), Xd(e) && Vd(t));
  }
  function qd(t, e, n) {
    return n(function () {
      Xd(e) && Vd(t);
    });
  }
  function Xd(t) {
    var e = t.getSnapshot;
    t = t.value;
    try {
      var n = e();
      return !je(t, n);
    } catch {
      return !0;
    }
  }
  function Vd(t) {
    var e = Xl(t, 2);
    e !== null && Ce(e, t, 2);
  }
  function pr(t) {
    var e = we();
    if (typeof t == "function") {
      var n = t;
      if (((t = n()), Jl)) {
        Se(!0);
        try {
          n();
        } finally {
          Se(!1);
        }
      }
    }
    return (
      (e.memoizedState = e.baseState = t),
      (e.queue = {
        pending: null,
        lanes: 0,
        dispatch: null,
        lastRenderedReducer: Rn,
        lastRenderedState: t,
      }),
      e
    );
  }
  function Ld(t, e, n, a) {
    return ((t.baseState = n), gr(t, Bt, typeof a == "function" ? a : Rn));
  }
  function Jp(t, e, n, a, c) {
    if (ac(t)) throw Error(s(485));
    if (((t = e.action), t !== null)) {
      var r = {
        payload: c,
        action: t,
        next: null,
        isTransition: !0,
        status: "pending",
        value: null,
        reason: null,
        listeners: [],
        then: function (h) {
          r.listeners.push(h);
        },
      };
      (S.T !== null ? n(!0) : (r.isTransition = !1),
        a(r),
        (n = e.pending),
        n === null
          ? ((r.next = e.pending = r), Gd(e, r))
          : ((r.next = n.next), (e.pending = n.next = r)));
    }
  }
  function Gd(t, e) {
    var n = e.action,
      a = e.payload,
      c = t.state;
    if (e.isTransition) {
      var r = S.T,
        h = {};
      S.T = h;
      try {
        var v = n(c, a),
          T = S.S;
        (T !== null && T(h, v), Zd(t, e, v));
      } catch (X) {
        vr(t, e, X);
      } finally {
        (r !== null && h.types !== null && (r.types = h.types), (S.T = r));
      }
    } else
      try {
        ((r = n(c, a)), Zd(t, e, r));
      } catch (X) {
        vr(t, e, X);
      }
  }
  function Zd(t, e, n) {
    n !== null && typeof n == "object" && typeof n.then == "function"
      ? n.then(
          function (a) {
            Qd(t, e, a);
          },
          function (a) {
            return vr(t, e, a);
          },
        )
      : Qd(t, e, n);
  }
  function Qd(t, e, n) {
    ((e.status = "fulfilled"),
      (e.value = n),
      kd(e),
      (t.state = n),
      (e = t.pending),
      e !== null &&
        ((n = e.next),
        n === e ? (t.pending = null) : ((n = n.next), (e.next = n), Gd(t, n))));
  }
  function vr(t, e, n) {
    var a = t.pending;
    if (((t.pending = null), a !== null)) {
      a = a.next;
      do ((e.status = "rejected"), (e.reason = n), kd(e), (e = e.next));
      while (e !== a);
    }
    t.action = null;
  }
  function kd(t) {
    t = t.listeners;
    for (var e = 0; e < t.length; e++) (0, t[e])();
  }
  function Kd(t, e) {
    return e;
  }
  function $d(t, e) {
    if (Ct) {
      var n = Xt.formState;
      if (n !== null) {
        t: {
          var a = Et;
          if (Ct) {
            if (Zt) {
              e: {
                for (var c = Zt, r = We; c.nodeType !== 8; ) {
                  if (!r) {
                    c = null;
                    break e;
                  }
                  if (((c = Ie(c.nextSibling)), c === null)) {
                    c = null;
                    break e;
                  }
                }
                ((r = c.data), (c = r === "F!" || r === "F" ? c : null));
              }
              if (c) {
                ((Zt = Ie(c.nextSibling)), (a = c.data === "F!"));
                break t;
              }
            }
            il(a);
          }
          a = !1;
        }
        a && (e = n[0]);
      }
    }
    return (
      (n = we()),
      (n.memoizedState = n.baseState = e),
      (a = {
        pending: null,
        lanes: 0,
        dispatch: null,
        lastRenderedReducer: Kd,
        lastRenderedState: e,
      }),
      (n.queue = a),
      (n = hh.bind(null, Et, a)),
      (a.dispatch = n),
      (a = pr(!1)),
      (r = wr.bind(null, Et, !1, a.queue)),
      (a = we()),
      (c = { state: e, dispatch: null, action: t, pending: null }),
      (a.queue = c),
      (n = Jp.bind(null, Et, c, r, n)),
      (c.dispatch = n),
      (a.memoizedState = t),
      [e, n, !1]
    );
  }
  function Jd(t) {
    var e = ee();
    return Wd(e, Bt, t);
  }
  function Wd(t, e, n) {
    if (
      ((e = gr(t, e, Kd)[0]),
      (t = ec(Rn)[0]),
      typeof e == "object" && e !== null && typeof e.then == "function")
    )
      try {
        var a = Nu(e);
      } catch (h) {
        throw h === Ea ? Qi : h;
      }
    else a = e;
    e = ee();
    var c = e.queue,
      r = c.dispatch;
    return (
      n !== e.memoizedState &&
        ((Et.flags |= 2048),
        Ta(9, { destroy: void 0 }, Wp.bind(null, c, n), null)),
      [a, r, t]
    );
  }
  function Wp(t, e) {
    t.action = e;
  }
  function Fd(t) {
    var e = ee(),
      n = Bt;
    if (n !== null) return Wd(e, n, t);
    (ee(), (e = e.memoizedState), (n = ee()));
    var a = n.queue.dispatch;
    return ((n.memoizedState = t), [e, a, !1]);
  }
  function Ta(t, e, n, a) {
    return (
      (t = { tag: t, create: n, deps: a, inst: e, next: null }),
      (e = Et.updateQueue),
      e === null && ((e = Pi()), (Et.updateQueue = e)),
      (n = e.lastEffect),
      n === null
        ? (e.lastEffect = t.next = t)
        : ((a = n.next), (n.next = t), (t.next = a), (e.lastEffect = t)),
      t
    );
  }
  function Id() {
    return ee().memoizedState;
  }
  function nc(t, e, n, a) {
    var c = we();
    ((Et.flags |= t),
      (c.memoizedState = Ta(
        1 | e,
        { destroy: void 0 },
        n,
        a === void 0 ? null : a,
      )));
  }
  function lc(t, e, n, a) {
    var c = ee();
    a = a === void 0 ? null : a;
    var r = c.memoizedState.inst;
    Bt !== null && a !== null && rr(a, Bt.memoizedState.deps)
      ? (c.memoizedState = Ta(e, r, n, a))
      : ((Et.flags |= t), (c.memoizedState = Ta(1 | e, r, n, a)));
  }
  function Pd(t, e) {
    nc(8390656, 8, t, e);
  }
  function xr(t, e) {
    lc(2048, 8, t, e);
  }
  function Fp(t) {
    Et.flags |= 4;
    var e = Et.updateQueue;
    if (e === null) ((e = Pi()), (Et.updateQueue = e), (e.events = [t]));
    else {
      var n = e.events;
      n === null ? (e.events = [t]) : n.push(t);
    }
  }
  function th(t) {
    var e = ee().memoizedState;
    return (
      Fp({ ref: e, nextImpl: t }),
      function () {
        if ((Ht & 2) !== 0) throw Error(s(440));
        return e.impl.apply(void 0, arguments);
      }
    );
  }
  function eh(t, e) {
    return lc(4, 2, t, e);
  }
  function nh(t, e) {
    return lc(4, 4, t, e);
  }
  function lh(t, e) {
    if (typeof e == "function") {
      t = t();
      var n = e(t);
      return function () {
        typeof n == "function" ? n() : e(null);
      };
    }
    if (e != null)
      return (
        (t = t()),
        (e.current = t),
        function () {
          e.current = null;
        }
      );
  }
  function ah(t, e, n) {
    ((n = n != null ? n.concat([t]) : null), lc(4, 4, lh.bind(null, e, t), n));
  }
  function br() {}
  function uh(t, e) {
    var n = ee();
    e = e === void 0 ? null : e;
    var a = n.memoizedState;
    return e !== null && rr(e, a[1]) ? a[0] : ((n.memoizedState = [t, e]), t);
  }
  function ih(t, e) {
    var n = ee();
    e = e === void 0 ? null : e;
    var a = n.memoizedState;
    if (e !== null && rr(e, a[1])) return a[0];
    if (((a = t()), Jl)) {
      Se(!0);
      try {
        t();
      } finally {
        Se(!1);
      }
    }
    return ((n.memoizedState = [a, e]), a);
  }
  function Sr(t, e, n) {
    return n === void 0 || ((On & 1073741824) !== 0 && (zt & 261930) === 0)
      ? (t.memoizedState = e)
      : ((t.memoizedState = n), (t = cm()), (Et.lanes |= t), (gl |= t), n);
  }
  function ch(t, e, n, a) {
    return je(n, e)
      ? n
      : wa.current !== null
        ? ((t = Sr(t, n, a)), je(t, e) || (ie = !0), t)
        : (On & 42) === 0 || ((On & 1073741824) !== 0 && (zt & 261930) === 0)
          ? ((ie = !0), (t.memoizedState = n))
          : ((t = cm()), (Et.lanes |= t), (gl |= t), e);
  }
  function sh(t, e, n, a, c) {
    var r = R.p;
    R.p = r !== 0 && 8 > r ? r : 8;
    var h = S.T,
      v = {};
    ((S.T = v), wr(t, !1, e, n));
    try {
      var T = c(),
        X = S.S;
      if (
        (X !== null && X(v, T),
        T !== null && typeof T == "object" && typeof T.then == "function")
      ) {
        var P = kp(T, a);
        Au(t, e, P, Ve(t));
      } else Au(t, e, a, Ve(t));
    } catch (lt) {
      Au(t, e, { then: function () {}, status: "rejected", reason: lt }, Ve());
    } finally {
      ((R.p = r),
        h !== null && v.types !== null && (h.types = v.types),
        (S.T = h));
    }
  }
  function Ip() {}
  function Er(t, e, n, a) {
    if (t.tag !== 5) throw Error(s(476));
    var c = rh(t).queue;
    sh(
      t,
      c,
      e,
      Z,
      n === null
        ? Ip
        : function () {
            return (oh(t), n(a));
          },
    );
  }
  function rh(t) {
    var e = t.memoizedState;
    if (e !== null) return e;
    e = {
      memoizedState: Z,
      baseState: Z,
      baseQueue: null,
      queue: {
        pending: null,
        lanes: 0,
        dispatch: null,
        lastRenderedReducer: Rn,
        lastRenderedState: Z,
      },
      next: null,
    };
    var n = {};
    return (
      (e.next = {
        memoizedState: n,
        baseState: n,
        baseQueue: null,
        queue: {
          pending: null,
          lanes: 0,
          dispatch: null,
          lastRenderedReducer: Rn,
          lastRenderedState: n,
        },
        next: null,
      }),
      (t.memoizedState = e),
      (t = t.alternate),
      t !== null && (t.memoizedState = e),
      e
    );
  }
  function oh(t) {
    var e = rh(t);
    (e.next === null && (e = t.alternate.memoizedState),
      Au(t, e.next.queue, {}, Ve()));
  }
  function _r() {
    return pe(Gu);
  }
  function fh() {
    return ee().memoizedState;
  }
  function dh() {
    return ee().memoizedState;
  }
  function Pp(t) {
    for (var e = t.return; e !== null; ) {
      switch (e.tag) {
        case 24:
        case 3:
          var n = Ve();
          t = rl(n);
          var a = ol(e, t, n);
          (a !== null && (Ce(a, e, n), Su(a, e, n)),
            (e = { cache: Is() }),
            (t.payload = e));
          return;
      }
      e = e.return;
    }
  }
  function tv(t, e, n) {
    var a = Ve();
    ((n = {
      lane: a,
      revertLane: 0,
      gesture: null,
      action: n,
      hasEagerState: !1,
      eagerState: null,
      next: null,
    }),
      ac(t)
        ? mh(e, n)
        : ((n = Vs(t, e, n, a)), n !== null && (Ce(n, t, a), gh(n, e, a))));
  }
  function hh(t, e, n) {
    var a = Ve();
    Au(t, e, n, a);
  }
  function Au(t, e, n, a) {
    var c = {
      lane: a,
      revertLane: 0,
      gesture: null,
      action: n,
      hasEagerState: !1,
      eagerState: null,
      next: null,
    };
    if (ac(t)) mh(e, c);
    else {
      var r = t.alternate;
      if (
        t.lanes === 0 &&
        (r === null || r.lanes === 0) &&
        ((r = e.lastRenderedReducer), r !== null)
      )
        try {
          var h = e.lastRenderedState,
            v = r(h, n);
          if (((c.hasEagerState = !0), (c.eagerState = v), je(v, h)))
            return (Yi(t, e, c, 0), Xt === null && Bi(), !1);
        } catch {
        } finally {
        }
      if (((n = Vs(t, e, c, a)), n !== null))
        return (Ce(n, t, a), gh(n, e, a), !0);
    }
    return !1;
  }
  function wr(t, e, n, a) {
    if (
      ((a = {
        lane: 2,
        revertLane: no(),
        gesture: null,
        action: a,
        hasEagerState: !1,
        eagerState: null,
        next: null,
      }),
      ac(t))
    ) {
      if (e) throw Error(s(479));
    } else ((e = Vs(t, n, a, 2)), e !== null && Ce(e, t, 2));
  }
  function ac(t) {
    var e = t.alternate;
    return t === Et || (e !== null && e === Et);
  }
  function mh(t, e) {
    Na = Fi = !0;
    var n = t.pending;
    (n === null ? (e.next = e) : ((e.next = n.next), (n.next = e)),
      (t.pending = e));
  }
  function gh(t, e, n) {
    if ((n & 4194048) !== 0) {
      var a = e.lanes;
      ((a &= t.pendingLanes), (n |= a), (e.lanes = n), pi(t, n));
    }
  }
  var Tu = {
    readContext: pe,
    use: tc,
    useCallback: Jt,
    useContext: Jt,
    useEffect: Jt,
    useImperativeHandle: Jt,
    useLayoutEffect: Jt,
    useInsertionEffect: Jt,
    useMemo: Jt,
    useReducer: Jt,
    useRef: Jt,
    useState: Jt,
    useDebugValue: Jt,
    useDeferredValue: Jt,
    useTransition: Jt,
    useSyncExternalStore: Jt,
    useId: Jt,
    useHostTransitionStatus: Jt,
    useFormState: Jt,
    useActionState: Jt,
    useOptimistic: Jt,
    useMemoCache: Jt,
    useCacheRefresh: Jt,
  };
  Tu.useEffectEvent = Jt;
  var yh = {
      readContext: pe,
      use: tc,
      useCallback: function (t, e) {
        return ((we().memoizedState = [t, e === void 0 ? null : e]), t);
      },
      useContext: pe,
      useEffect: Pd,
      useImperativeHandle: function (t, e, n) {
        ((n = n != null ? n.concat([t]) : null),
          nc(4194308, 4, lh.bind(null, e, t), n));
      },
      useLayoutEffect: function (t, e) {
        return nc(4194308, 4, t, e);
      },
      useInsertionEffect: function (t, e) {
        nc(4, 2, t, e);
      },
      useMemo: function (t, e) {
        var n = we();
        e = e === void 0 ? null : e;
        var a = t();
        if (Jl) {
          Se(!0);
          try {
            t();
          } finally {
            Se(!1);
          }
        }
        return ((n.memoizedState = [a, e]), a);
      },
      useReducer: function (t, e, n) {
        var a = we();
        if (n !== void 0) {
          var c = n(e);
          if (Jl) {
            Se(!0);
            try {
              n(e);
            } finally {
              Se(!1);
            }
          }
        } else c = e;
        return (
          (a.memoizedState = a.baseState = c),
          (t = {
            pending: null,
            lanes: 0,
            dispatch: null,
            lastRenderedReducer: t,
            lastRenderedState: c,
          }),
          (a.queue = t),
          (t = t.dispatch = tv.bind(null, Et, t)),
          [a.memoizedState, t]
        );
      },
      useRef: function (t) {
        var e = we();
        return ((t = { current: t }), (e.memoizedState = t));
      },
      useState: function (t) {
        t = pr(t);
        var e = t.queue,
          n = hh.bind(null, Et, e);
        return ((e.dispatch = n), [t.memoizedState, n]);
      },
      useDebugValue: br,
      useDeferredValue: function (t, e) {
        var n = we();
        return Sr(n, t, e);
      },
      useTransition: function () {
        var t = pr(!1);
        return (
          (t = sh.bind(null, Et, t.queue, !0, !1)),
          (we().memoizedState = t),
          [!1, t]
        );
      },
      useSyncExternalStore: function (t, e, n) {
        var a = Et,
          c = we();
        if (Ct) {
          if (n === void 0) throw Error(s(407));
          n = n();
        } else {
          if (((n = e()), Xt === null)) throw Error(s(349));
          (zt & 127) !== 0 || Bd(a, e, n);
        }
        c.memoizedState = n;
        var r = { value: n, getSnapshot: e };
        return (
          (c.queue = r),
          Pd(qd.bind(null, a, r, t), [t]),
          (a.flags |= 2048),
          Ta(9, { destroy: void 0 }, Yd.bind(null, a, r, n, e), null),
          n
        );
      },
      useId: function () {
        var t = we(),
          e = Xt.identifierPrefix;
        if (Ct) {
          var n = yn,
            a = gn;
          ((n = (a & ~(1 << (32 - $t(a) - 1))).toString(32) + n),
            (e = "_" + e + "R_" + n),
            (n = Ii++),
            0 < n && (e += "H" + n.toString(32)),
            (e += "_"));
        } else ((n = Kp++), (e = "_" + e + "r_" + n.toString(32) + "_"));
        return (t.memoizedState = e);
      },
      useHostTransitionStatus: _r,
      useFormState: $d,
      useActionState: $d,
      useOptimistic: function (t) {
        var e = we();
        e.memoizedState = e.baseState = t;
        var n = {
          pending: null,
          lanes: 0,
          dispatch: null,
          lastRenderedReducer: null,
          lastRenderedState: null,
        };
        return (
          (e.queue = n),
          (e = wr.bind(null, Et, !0, n)),
          (n.dispatch = e),
          [t, e]
        );
      },
      useMemoCache: mr,
      useCacheRefresh: function () {
        return (we().memoizedState = Pp.bind(null, Et));
      },
      useEffectEvent: function (t) {
        var e = we(),
          n = { impl: t };
        return (
          (e.memoizedState = n),
          function () {
            if ((Ht & 2) !== 0) throw Error(s(440));
            return n.impl.apply(void 0, arguments);
          }
        );
      },
    },
    Nr = {
      readContext: pe,
      use: tc,
      useCallback: uh,
      useContext: pe,
      useEffect: xr,
      useImperativeHandle: ah,
      useInsertionEffect: eh,
      useLayoutEffect: nh,
      useMemo: ih,
      useReducer: ec,
      useRef: Id,
      useState: function () {
        return ec(Rn);
      },
      useDebugValue: br,
      useDeferredValue: function (t, e) {
        var n = ee();
        return ch(n, Bt.memoizedState, t, e);
      },
      useTransition: function () {
        var t = ec(Rn)[0],
          e = ee().memoizedState;
        return [typeof t == "boolean" ? t : Nu(t), e];
      },
      useSyncExternalStore: Ud,
      useId: fh,
      useHostTransitionStatus: _r,
      useFormState: Jd,
      useActionState: Jd,
      useOptimistic: function (t, e) {
        var n = ee();
        return Ld(n, Bt, t, e);
      },
      useMemoCache: mr,
      useCacheRefresh: dh,
    };
  Nr.useEffectEvent = th;
  var ph = {
    readContext: pe,
    use: tc,
    useCallback: uh,
    useContext: pe,
    useEffect: xr,
    useImperativeHandle: ah,
    useInsertionEffect: eh,
    useLayoutEffect: nh,
    useMemo: ih,
    useReducer: yr,
    useRef: Id,
    useState: function () {
      return yr(Rn);
    },
    useDebugValue: br,
    useDeferredValue: function (t, e) {
      var n = ee();
      return Bt === null ? Sr(n, t, e) : ch(n, Bt.memoizedState, t, e);
    },
    useTransition: function () {
      var t = yr(Rn)[0],
        e = ee().memoizedState;
      return [typeof t == "boolean" ? t : Nu(t), e];
    },
    useSyncExternalStore: Ud,
    useId: fh,
    useHostTransitionStatus: _r,
    useFormState: Fd,
    useActionState: Fd,
    useOptimistic: function (t, e) {
      var n = ee();
      return Bt !== null
        ? Ld(n, Bt, t, e)
        : ((n.baseState = t), [t, n.queue.dispatch]);
    },
    useMemoCache: mr,
    useCacheRefresh: dh,
  };
  ph.useEffectEvent = th;
  function Ar(t, e, n, a) {
    ((e = t.memoizedState),
      (n = n(a, e)),
      (n = n == null ? e : x({}, e, n)),
      (t.memoizedState = n),
      t.lanes === 0 && (t.updateQueue.baseState = n));
  }
  var Tr = {
    enqueueSetState: function (t, e, n) {
      t = t._reactInternals;
      var a = Ve(),
        c = rl(a);
      ((c.payload = e),
        n != null && (c.callback = n),
        (e = ol(t, c, a)),
        e !== null && (Ce(e, t, a), Su(e, t, a)));
    },
    enqueueReplaceState: function (t, e, n) {
      t = t._reactInternals;
      var a = Ve(),
        c = rl(a);
      ((c.tag = 1),
        (c.payload = e),
        n != null && (c.callback = n),
        (e = ol(t, c, a)),
        e !== null && (Ce(e, t, a), Su(e, t, a)));
    },
    enqueueForceUpdate: function (t, e) {
      t = t._reactInternals;
      var n = Ve(),
        a = rl(n);
      ((a.tag = 2),
        e != null && (a.callback = e),
        (e = ol(t, a, n)),
        e !== null && (Ce(e, t, n), Su(e, t, n)));
    },
  };
  function vh(t, e, n, a, c, r, h) {
    return (
      (t = t.stateNode),
      typeof t.shouldComponentUpdate == "function"
        ? t.shouldComponentUpdate(a, r, h)
        : e.prototype && e.prototype.isPureReactComponent
          ? !hu(n, a) || !hu(c, r)
          : !0
    );
  }
  function xh(t, e, n, a) {
    ((t = e.state),
      typeof e.componentWillReceiveProps == "function" &&
        e.componentWillReceiveProps(n, a),
      typeof e.UNSAFE_componentWillReceiveProps == "function" &&
        e.UNSAFE_componentWillReceiveProps(n, a),
      e.state !== t && Tr.enqueueReplaceState(e, e.state, null));
  }
  function Wl(t, e) {
    var n = e;
    if ("ref" in e) {
      n = {};
      for (var a in e) a !== "ref" && (n[a] = e[a]);
    }
    if ((t = t.defaultProps)) {
      n === e && (n = x({}, n));
      for (var c in t) n[c] === void 0 && (n[c] = t[c]);
    }
    return n;
  }
  function bh(t) {
    Ui(t);
  }
  function Sh(t) {
    console.error(t);
  }
  function Eh(t) {
    Ui(t);
  }
  function uc(t, e) {
    try {
      var n = t.onUncaughtError;
      n(e.value, { componentStack: e.stack });
    } catch (a) {
      setTimeout(function () {
        throw a;
      });
    }
  }
  function _h(t, e, n) {
    try {
      var a = t.onCaughtError;
      a(n.value, {
        componentStack: n.stack,
        errorBoundary: e.tag === 1 ? e.stateNode : null,
      });
    } catch (c) {
      setTimeout(function () {
        throw c;
      });
    }
  }
  function zr(t, e, n) {
    return (
      (n = rl(n)),
      (n.tag = 3),
      (n.payload = { element: null }),
      (n.callback = function () {
        uc(t, e);
      }),
      n
    );
  }
  function wh(t) {
    return ((t = rl(t)), (t.tag = 3), t);
  }
  function Nh(t, e, n, a) {
    var c = n.type.getDerivedStateFromError;
    if (typeof c == "function") {
      var r = a.value;
      ((t.payload = function () {
        return c(r);
      }),
        (t.callback = function () {
          _h(e, n, a);
        }));
    }
    var h = n.stateNode;
    h !== null &&
      typeof h.componentDidCatch == "function" &&
      (t.callback = function () {
        (_h(e, n, a),
          typeof c != "function" &&
            (yl === null ? (yl = new Set([this])) : yl.add(this)));
        var v = a.stack;
        this.componentDidCatch(a.value, {
          componentStack: v !== null ? v : "",
        });
      });
  }
  function ev(t, e, n, a, c) {
    if (
      ((n.flags |= 32768),
      a !== null && typeof a == "object" && typeof a.then == "function")
    ) {
      if (
        ((e = n.alternate),
        e !== null && xa(e, n, c, !0),
        (n = Be.current),
        n !== null)
      ) {
        switch (n.tag) {
          case 31:
          case 13:
            return (
              Fe === null ? pc() : n.alternate === null && Wt === 0 && (Wt = 3),
              (n.flags &= -257),
              (n.flags |= 65536),
              (n.lanes = c),
              a === ki
                ? (n.flags |= 16384)
                : ((e = n.updateQueue),
                  e === null ? (n.updateQueue = new Set([a])) : e.add(a),
                  Pr(t, a, c)),
              !1
            );
          case 22:
            return (
              (n.flags |= 65536),
              a === ki
                ? (n.flags |= 16384)
                : ((e = n.updateQueue),
                  e === null
                    ? ((e = {
                        transitions: null,
                        markerInstances: null,
                        retryQueue: new Set([a]),
                      }),
                      (n.updateQueue = e))
                    : ((n = e.retryQueue),
                      n === null ? (e.retryQueue = new Set([a])) : n.add(a)),
                  Pr(t, a, c)),
              !1
            );
        }
        throw Error(s(435, n.tag));
      }
      return (Pr(t, a, c), pc(), !1);
    }
    if (Ct)
      return (
        (e = Be.current),
        e !== null
          ? ((e.flags & 65536) === 0 && (e.flags |= 256),
            (e.flags |= 65536),
            (e.lanes = c),
            a !== Ks && ((t = Error(s(422), { cause: a })), yu(Ke(t, n))))
          : (a !== Ks && ((e = Error(s(423), { cause: a })), yu(Ke(e, n))),
            (t = t.current.alternate),
            (t.flags |= 65536),
            (c &= -c),
            (t.lanes |= c),
            (a = Ke(a, n)),
            (c = zr(t.stateNode, a, c)),
            ar(t, c),
            Wt !== 4 && (Wt = 2)),
        !1
      );
    var r = Error(s(520), { cause: a });
    if (
      ((r = Ke(r, n)),
      ju === null ? (ju = [r]) : ju.push(r),
      Wt !== 4 && (Wt = 2),
      e === null)
    )
      return !0;
    ((a = Ke(a, n)), (n = e));
    do {
      switch (n.tag) {
        case 3:
          return (
            (n.flags |= 65536),
            (t = c & -c),
            (n.lanes |= t),
            (t = zr(n.stateNode, a, t)),
            ar(n, t),
            !1
          );
        case 1:
          if (
            ((e = n.type),
            (r = n.stateNode),
            (n.flags & 128) === 0 &&
              (typeof e.getDerivedStateFromError == "function" ||
                (r !== null &&
                  typeof r.componentDidCatch == "function" &&
                  (yl === null || !yl.has(r)))))
          )
            return (
              (n.flags |= 65536),
              (c &= -c),
              (n.lanes |= c),
              (c = wh(c)),
              Nh(c, t, n, a),
              ar(n, c),
              !1
            );
      }
      n = n.return;
    } while (n !== null);
    return !1;
  }
  var Mr = Error(s(461)),
    ie = !1;
  function ve(t, e, n, a) {
    e.child = t === null ? Md(e, null, n, a) : $l(e, t.child, n, a);
  }
  function Ah(t, e, n, a, c) {
    n = n.render;
    var r = e.ref;
    if ("ref" in a) {
      var h = {};
      for (var v in a) v !== "ref" && (h[v] = a[v]);
    } else h = a;
    return (
      Zl(e),
      (a = or(t, e, n, h, r, c)),
      (v = fr()),
      t !== null && !ie
        ? (dr(t, e, c), Hn(t, e, c))
        : (Ct && v && Qs(e), (e.flags |= 1), ve(t, e, a, c), e.child)
    );
  }
  function Th(t, e, n, a, c) {
    if (t === null) {
      var r = n.type;
      return typeof r == "function" &&
        !Ls(r) &&
        r.defaultProps === void 0 &&
        n.compare === null
        ? ((e.tag = 15), (e.type = r), zh(t, e, r, a, c))
        : ((t = Xi(n.type, null, a, e, e.mode, c)),
          (t.ref = e.ref),
          (t.return = e),
          (e.child = t));
    }
    if (((r = t.child), !Br(t, c))) {
      var h = r.memoizedProps;
      if (
        ((n = n.compare), (n = n !== null ? n : hu), n(h, a) && t.ref === e.ref)
      )
        return Hn(t, e, c);
    }
    return (
      (e.flags |= 1),
      (t = zn(r, a)),
      (t.ref = e.ref),
      (t.return = e),
      (e.child = t)
    );
  }
  function zh(t, e, n, a, c) {
    if (t !== null) {
      var r = t.memoizedProps;
      if (hu(r, a) && t.ref === e.ref)
        if (((ie = !1), (e.pendingProps = a = r), Br(t, c)))
          (t.flags & 131072) !== 0 && (ie = !0);
        else return ((e.lanes = t.lanes), Hn(t, e, c));
    }
    return Cr(t, e, n, a, c);
  }
  function Mh(t, e, n, a) {
    var c = a.children,
      r = t !== null ? t.memoizedState : null;
    if (
      (t === null &&
        e.stateNode === null &&
        (e.stateNode = {
          _visibility: 1,
          _pendingMarkers: null,
          _retryCache: null,
          _transitions: null,
        }),
      a.mode === "hidden")
    ) {
      if ((e.flags & 128) !== 0) {
        if (((r = r !== null ? r.baseLanes | n : n), t !== null)) {
          for (a = e.child = t.child, c = 0; a !== null; )
            ((c = c | a.lanes | a.childLanes), (a = a.sibling));
          a = c & ~r;
        } else ((a = 0), (e.child = null));
        return Ch(t, e, r, n, a);
      }
      if ((n & 536870912) !== 0)
        ((e.memoizedState = { baseLanes: 0, cachePool: null }),
          t !== null && Zi(e, r !== null ? r.cachePool : null),
          r !== null ? Od(e, r) : ir(),
          Rd(e));
      else
        return (
          (a = e.lanes = 536870912),
          Ch(t, e, r !== null ? r.baseLanes | n : n, n, a)
        );
    } else
      r !== null
        ? (Zi(e, r.cachePool), Od(e, r), dl(), (e.memoizedState = null))
        : (t !== null && Zi(e, null), ir(), dl());
    return (ve(t, e, c, n), e.child);
  }
  function zu(t, e) {
    return (
      (t !== null && t.tag === 22) ||
        e.stateNode !== null ||
        (e.stateNode = {
          _visibility: 1,
          _pendingMarkers: null,
          _retryCache: null,
          _transitions: null,
        }),
      e.sibling
    );
  }
  function Ch(t, e, n, a, c) {
    var r = tr();
    return (
      (r = r === null ? null : { parent: ae._currentValue, pool: r }),
      (e.memoizedState = { baseLanes: n, cachePool: r }),
      t !== null && Zi(e, null),
      ir(),
      Rd(e),
      t !== null && xa(t, e, a, !0),
      (e.childLanes = c),
      null
    );
  }
  function ic(t, e) {
    return (
      (e = sc({ mode: e.mode, children: e.children }, t.mode)),
      (e.ref = t.ref),
      (t.child = e),
      (e.return = t),
      e
    );
  }
  function Dh(t, e, n) {
    return (
      $l(e, t.child, null, n),
      (t = ic(e, e.pendingProps)),
      (t.flags |= 2),
      Ye(e),
      (e.memoizedState = null),
      t
    );
  }
  function nv(t, e, n) {
    var a = e.pendingProps,
      c = (e.flags & 128) !== 0;
    if (((e.flags &= -129), t === null)) {
      if (Ct) {
        if (a.mode === "hidden")
          return ((t = ic(e, a)), (e.lanes = 536870912), zu(null, t));
        if (
          (sr(e),
          (t = Zt)
            ? ((t = Gm(t, We)),
              (t = t !== null && t.data === "&" ? t : null),
              t !== null &&
                ((e.memoizedState = {
                  dehydrated: t,
                  treeContext: al !== null ? { id: gn, overflow: yn } : null,
                  retryLane: 536870912,
                  hydrationErrors: null,
                }),
                (n = md(t)),
                (n.return = e),
                (e.child = n),
                (ye = e),
                (Zt = null)))
            : (t = null),
          t === null)
        )
          throw il(e);
        return ((e.lanes = 536870912), null);
      }
      return ic(e, a);
    }
    var r = t.memoizedState;
    if (r !== null) {
      var h = r.dehydrated;
      if ((sr(e), c))
        if (e.flags & 256) ((e.flags &= -257), (e = Dh(t, e, n)));
        else if (e.memoizedState !== null)
          ((e.child = t.child), (e.flags |= 128), (e = null));
        else throw Error(s(558));
      else if (
        (ie || xa(t, e, n, !1), (c = (n & t.childLanes) !== 0), ie || c)
      ) {
        if (
          ((a = Xt),
          a !== null && ((h = vi(a, n)), h !== 0 && h !== r.retryLane))
        )
          throw ((r.retryLane = h), Xl(t, h), Ce(a, t, h), Mr);
        (pc(), (e = Dh(t, e, n)));
      } else
        ((t = r.treeContext),
          (Zt = Ie(h.nextSibling)),
          (ye = e),
          (Ct = !0),
          (ul = null),
          (We = !1),
          t !== null && pd(e, t),
          (e = ic(e, a)),
          (e.flags |= 4096));
      return e;
    }
    return (
      (t = zn(t.child, { mode: a.mode, children: a.children })),
      (t.ref = e.ref),
      (e.child = t),
      (t.return = e),
      t
    );
  }
  function cc(t, e) {
    var n = e.ref;
    if (n === null) t !== null && t.ref !== null && (e.flags |= 4194816);
    else {
      if (typeof n != "function" && typeof n != "object") throw Error(s(284));
      (t === null || t.ref !== n) && (e.flags |= 4194816);
    }
  }
  function Cr(t, e, n, a, c) {
    return (
      Zl(e),
      (n = or(t, e, n, a, void 0, c)),
      (a = fr()),
      t !== null && !ie
        ? (dr(t, e, c), Hn(t, e, c))
        : (Ct && a && Qs(e), (e.flags |= 1), ve(t, e, n, c), e.child)
    );
  }
  function Oh(t, e, n, a, c, r) {
    return (
      Zl(e),
      (e.updateQueue = null),
      (n = jd(e, a, n, c)),
      Hd(t),
      (a = fr()),
      t !== null && !ie
        ? (dr(t, e, r), Hn(t, e, r))
        : (Ct && a && Qs(e), (e.flags |= 1), ve(t, e, n, r), e.child)
    );
  }
  function Rh(t, e, n, a, c) {
    if ((Zl(e), e.stateNode === null)) {
      var r = ga,
        h = n.contextType;
      (typeof h == "object" && h !== null && (r = pe(h)),
        (r = new n(a, r)),
        (e.memoizedState =
          r.state !== null && r.state !== void 0 ? r.state : null),
        (r.updater = Tr),
        (e.stateNode = r),
        (r._reactInternals = e),
        (r = e.stateNode),
        (r.props = a),
        (r.state = e.memoizedState),
        (r.refs = {}),
        nr(e),
        (h = n.contextType),
        (r.context = typeof h == "object" && h !== null ? pe(h) : ga),
        (r.state = e.memoizedState),
        (h = n.getDerivedStateFromProps),
        typeof h == "function" && (Ar(e, n, h, a), (r.state = e.memoizedState)),
        typeof n.getDerivedStateFromProps == "function" ||
          typeof r.getSnapshotBeforeUpdate == "function" ||
          (typeof r.UNSAFE_componentWillMount != "function" &&
            typeof r.componentWillMount != "function") ||
          ((h = r.state),
          typeof r.componentWillMount == "function" && r.componentWillMount(),
          typeof r.UNSAFE_componentWillMount == "function" &&
            r.UNSAFE_componentWillMount(),
          h !== r.state && Tr.enqueueReplaceState(r, r.state, null),
          _u(e, a, r, c),
          Eu(),
          (r.state = e.memoizedState)),
        typeof r.componentDidMount == "function" && (e.flags |= 4194308),
        (a = !0));
    } else if (t === null) {
      r = e.stateNode;
      var v = e.memoizedProps,
        T = Wl(n, v);
      r.props = T;
      var X = r.context,
        P = n.contextType;
      ((h = ga), typeof P == "object" && P !== null && (h = pe(P)));
      var lt = n.getDerivedStateFromProps;
      ((P =
        typeof lt == "function" ||
        typeof r.getSnapshotBeforeUpdate == "function"),
        (v = e.pendingProps !== v),
        P ||
          (typeof r.UNSAFE_componentWillReceiveProps != "function" &&
            typeof r.componentWillReceiveProps != "function") ||
          ((v || X !== h) && xh(e, r, a, h)),
        (sl = !1));
      var V = e.memoizedState;
      ((r.state = V),
        _u(e, a, r, c),
        Eu(),
        (X = e.memoizedState),
        v || V !== X || sl
          ? (typeof lt == "function" &&
              (Ar(e, n, lt, a), (X = e.memoizedState)),
            (T = sl || vh(e, n, T, a, V, X, h))
              ? (P ||
                  (typeof r.UNSAFE_componentWillMount != "function" &&
                    typeof r.componentWillMount != "function") ||
                  (typeof r.componentWillMount == "function" &&
                    r.componentWillMount(),
                  typeof r.UNSAFE_componentWillMount == "function" &&
                    r.UNSAFE_componentWillMount()),
                typeof r.componentDidMount == "function" &&
                  (e.flags |= 4194308))
              : (typeof r.componentDidMount == "function" &&
                  (e.flags |= 4194308),
                (e.memoizedProps = a),
                (e.memoizedState = X)),
            (r.props = a),
            (r.state = X),
            (r.context = h),
            (a = T))
          : (typeof r.componentDidMount == "function" && (e.flags |= 4194308),
            (a = !1)));
    } else {
      ((r = e.stateNode),
        lr(t, e),
        (h = e.memoizedProps),
        (P = Wl(n, h)),
        (r.props = P),
        (lt = e.pendingProps),
        (V = r.context),
        (X = n.contextType),
        (T = ga),
        typeof X == "object" && X !== null && (T = pe(X)),
        (v = n.getDerivedStateFromProps),
        (X =
          typeof v == "function" ||
          typeof r.getSnapshotBeforeUpdate == "function") ||
          (typeof r.UNSAFE_componentWillReceiveProps != "function" &&
            typeof r.componentWillReceiveProps != "function") ||
          ((h !== lt || V !== T) && xh(e, r, a, T)),
        (sl = !1),
        (V = e.memoizedState),
        (r.state = V),
        _u(e, a, r, c),
        Eu());
      var K = e.memoizedState;
      h !== lt ||
      V !== K ||
      sl ||
      (t !== null && t.dependencies !== null && Li(t.dependencies))
        ? (typeof v == "function" && (Ar(e, n, v, a), (K = e.memoizedState)),
          (P =
            sl ||
            vh(e, n, P, a, V, K, T) ||
            (t !== null && t.dependencies !== null && Li(t.dependencies)))
            ? (X ||
                (typeof r.UNSAFE_componentWillUpdate != "function" &&
                  typeof r.componentWillUpdate != "function") ||
                (typeof r.componentWillUpdate == "function" &&
                  r.componentWillUpdate(a, K, T),
                typeof r.UNSAFE_componentWillUpdate == "function" &&
                  r.UNSAFE_componentWillUpdate(a, K, T)),
              typeof r.componentDidUpdate == "function" && (e.flags |= 4),
              typeof r.getSnapshotBeforeUpdate == "function" &&
                (e.flags |= 1024))
            : (typeof r.componentDidUpdate != "function" ||
                (h === t.memoizedProps && V === t.memoizedState) ||
                (e.flags |= 4),
              typeof r.getSnapshotBeforeUpdate != "function" ||
                (h === t.memoizedProps && V === t.memoizedState) ||
                (e.flags |= 1024),
              (e.memoizedProps = a),
              (e.memoizedState = K)),
          (r.props = a),
          (r.state = K),
          (r.context = T),
          (a = P))
        : (typeof r.componentDidUpdate != "function" ||
            (h === t.memoizedProps && V === t.memoizedState) ||
            (e.flags |= 4),
          typeof r.getSnapshotBeforeUpdate != "function" ||
            (h === t.memoizedProps && V === t.memoizedState) ||
            (e.flags |= 1024),
          (a = !1));
    }
    return (
      (r = a),
      cc(t, e),
      (a = (e.flags & 128) !== 0),
      r || a
        ? ((r = e.stateNode),
          (n =
            a && typeof n.getDerivedStateFromError != "function"
              ? null
              : r.render()),
          (e.flags |= 1),
          t !== null && a
            ? ((e.child = $l(e, t.child, null, c)),
              (e.child = $l(e, null, n, c)))
            : ve(t, e, n, c),
          (e.memoizedState = r.state),
          (t = e.child))
        : (t = Hn(t, e, c)),
      t
    );
  }
  function Hh(t, e, n, a) {
    return (Ll(), (e.flags |= 256), ve(t, e, n, a), e.child);
  }
  var Dr = {
    dehydrated: null,
    treeContext: null,
    retryLane: 0,
    hydrationErrors: null,
  };
  function Or(t) {
    return { baseLanes: t, cachePool: _d() };
  }
  function Rr(t, e, n) {
    return ((t = t !== null ? t.childLanes & ~n : 0), e && (t |= Xe), t);
  }
  function jh(t, e, n) {
    var a = e.pendingProps,
      c = !1,
      r = (e.flags & 128) !== 0,
      h;
    if (
      ((h = r) ||
        (h =
          t !== null && t.memoizedState === null ? !1 : (te.current & 2) !== 0),
      h && ((c = !0), (e.flags &= -129)),
      (h = (e.flags & 32) !== 0),
      (e.flags &= -33),
      t === null)
    ) {
      if (Ct) {
        if (
          (c ? fl(e) : dl(),
          (t = Zt)
            ? ((t = Gm(t, We)),
              (t = t !== null && t.data !== "&" ? t : null),
              t !== null &&
                ((e.memoizedState = {
                  dehydrated: t,
                  treeContext: al !== null ? { id: gn, overflow: yn } : null,
                  retryLane: 536870912,
                  hydrationErrors: null,
                }),
                (n = md(t)),
                (n.return = e),
                (e.child = n),
                (ye = e),
                (Zt = null)))
            : (t = null),
          t === null)
        )
          throw il(e);
        return (yo(t) ? (e.lanes = 32) : (e.lanes = 536870912), null);
      }
      var v = a.children;
      return (
        (a = a.fallback),
        c
          ? (dl(),
            (c = e.mode),
            (v = sc({ mode: "hidden", children: v }, c)),
            (a = Vl(a, c, n, null)),
            (v.return = e),
            (a.return = e),
            (v.sibling = a),
            (e.child = v),
            (a = e.child),
            (a.memoizedState = Or(n)),
            (a.childLanes = Rr(t, h, n)),
            (e.memoizedState = Dr),
            zu(null, a))
          : (fl(e), Hr(e, v))
      );
    }
    var T = t.memoizedState;
    if (T !== null && ((v = T.dehydrated), v !== null)) {
      if (r)
        e.flags & 256
          ? (fl(e), (e.flags &= -257), (e = jr(t, e, n)))
          : e.memoizedState !== null
            ? (dl(), (e.child = t.child), (e.flags |= 128), (e = null))
            : (dl(),
              (v = a.fallback),
              (c = e.mode),
              (a = sc({ mode: "visible", children: a.children }, c)),
              (v = Vl(v, c, n, null)),
              (v.flags |= 2),
              (a.return = e),
              (v.return = e),
              (a.sibling = v),
              (e.child = a),
              $l(e, t.child, null, n),
              (a = e.child),
              (a.memoizedState = Or(n)),
              (a.childLanes = Rr(t, h, n)),
              (e.memoizedState = Dr),
              (e = zu(null, a)));
      else if ((fl(e), yo(v))) {
        if (((h = v.nextSibling && v.nextSibling.dataset), h)) var X = h.dgst;
        ((h = X),
          (a = Error(s(419))),
          (a.stack = ""),
          (a.digest = h),
          yu({ value: a, source: null, stack: null }),
          (e = jr(t, e, n)));
      } else if (
        (ie || xa(t, e, n, !1), (h = (n & t.childLanes) !== 0), ie || h)
      ) {
        if (
          ((h = Xt),
          h !== null && ((a = vi(h, n)), a !== 0 && a !== T.retryLane))
        )
          throw ((T.retryLane = a), Xl(t, a), Ce(h, t, a), Mr);
        (go(v) || pc(), (e = jr(t, e, n)));
      } else
        go(v)
          ? ((e.flags |= 192), (e.child = t.child), (e = null))
          : ((t = T.treeContext),
            (Zt = Ie(v.nextSibling)),
            (ye = e),
            (Ct = !0),
            (ul = null),
            (We = !1),
            t !== null && pd(e, t),
            (e = Hr(e, a.children)),
            (e.flags |= 4096));
      return e;
    }
    return c
      ? (dl(),
        (v = a.fallback),
        (c = e.mode),
        (T = t.child),
        (X = T.sibling),
        (a = zn(T, { mode: "hidden", children: a.children })),
        (a.subtreeFlags = T.subtreeFlags & 65011712),
        X !== null ? (v = zn(X, v)) : ((v = Vl(v, c, n, null)), (v.flags |= 2)),
        (v.return = e),
        (a.return = e),
        (a.sibling = v),
        (e.child = a),
        zu(null, a),
        (a = e.child),
        (v = t.child.memoizedState),
        v === null
          ? (v = Or(n))
          : ((c = v.cachePool),
            c !== null
              ? ((T = ae._currentValue),
                (c = c.parent !== T ? { parent: T, pool: T } : c))
              : (c = _d()),
            (v = { baseLanes: v.baseLanes | n, cachePool: c })),
        (a.memoizedState = v),
        (a.childLanes = Rr(t, h, n)),
        (e.memoizedState = Dr),
        zu(t.child, a))
      : (fl(e),
        (n = t.child),
        (t = n.sibling),
        (n = zn(n, { mode: "visible", children: a.children })),
        (n.return = e),
        (n.sibling = null),
        t !== null &&
          ((h = e.deletions),
          h === null ? ((e.deletions = [t]), (e.flags |= 16)) : h.push(t)),
        (e.child = n),
        (e.memoizedState = null),
        n);
  }
  function Hr(t, e) {
    return (
      (e = sc({ mode: "visible", children: e }, t.mode)),
      (e.return = t),
      (t.child = e)
    );
  }
  function sc(t, e) {
    return ((t = Ue(22, t, null, e)), (t.lanes = 0), t);
  }
  function jr(t, e, n) {
    return (
      $l(e, t.child, null, n),
      (t = Hr(e, e.pendingProps.children)),
      (t.flags |= 2),
      (e.memoizedState = null),
      t
    );
  }
  function Uh(t, e, n) {
    t.lanes |= e;
    var a = t.alternate;
    (a !== null && (a.lanes |= e), Ws(t.return, e, n));
  }
  function Ur(t, e, n, a, c, r) {
    var h = t.memoizedState;
    h === null
      ? (t.memoizedState = {
          isBackwards: e,
          rendering: null,
          renderingStartTime: 0,
          last: a,
          tail: n,
          tailMode: c,
          treeForkCount: r,
        })
      : ((h.isBackwards = e),
        (h.rendering = null),
        (h.renderingStartTime = 0),
        (h.last = a),
        (h.tail = n),
        (h.tailMode = c),
        (h.treeForkCount = r));
  }
  function Bh(t, e, n) {
    var a = e.pendingProps,
      c = a.revealOrder,
      r = a.tail;
    a = a.children;
    var h = te.current,
      v = (h & 2) !== 0;
    if (
      (v ? ((h = (h & 1) | 2), (e.flags |= 128)) : (h &= 1),
      $(te, h),
      ve(t, e, a, n),
      (a = Ct ? gu : 0),
      !v && t !== null && (t.flags & 128) !== 0)
    )
      t: for (t = e.child; t !== null; ) {
        if (t.tag === 13) t.memoizedState !== null && Uh(t, n, e);
        else if (t.tag === 19) Uh(t, n, e);
        else if (t.child !== null) {
          ((t.child.return = t), (t = t.child));
          continue;
        }
        if (t === e) break t;
        for (; t.sibling === null; ) {
          if (t.return === null || t.return === e) break t;
          t = t.return;
        }
        ((t.sibling.return = t.return), (t = t.sibling));
      }
    switch (c) {
      case "forwards":
        for (n = e.child, c = null; n !== null; )
          ((t = n.alternate),
            t !== null && Wi(t) === null && (c = n),
            (n = n.sibling));
        ((n = c),
          n === null
            ? ((c = e.child), (e.child = null))
            : ((c = n.sibling), (n.sibling = null)),
          Ur(e, !1, c, n, r, a));
        break;
      case "backwards":
      case "unstable_legacy-backwards":
        for (n = null, c = e.child, e.child = null; c !== null; ) {
          if (((t = c.alternate), t !== null && Wi(t) === null)) {
            e.child = c;
            break;
          }
          ((t = c.sibling), (c.sibling = n), (n = c), (c = t));
        }
        Ur(e, !0, n, null, r, a);
        break;
      case "together":
        Ur(e, !1, null, null, void 0, a);
        break;
      default:
        e.memoizedState = null;
    }
    return e.child;
  }
  function Hn(t, e, n) {
    if (
      (t !== null && (e.dependencies = t.dependencies),
      (gl |= e.lanes),
      (n & e.childLanes) === 0)
    )
      if (t !== null) {
        if ((xa(t, e, n, !1), (n & e.childLanes) === 0)) return null;
      } else return null;
    if (t !== null && e.child !== t.child) throw Error(s(153));
    if (e.child !== null) {
      for (
        t = e.child, n = zn(t, t.pendingProps), e.child = n, n.return = e;
        t.sibling !== null;
      )
        ((t = t.sibling),
          (n = n.sibling = zn(t, t.pendingProps)),
          (n.return = e));
      n.sibling = null;
    }
    return e.child;
  }
  function Br(t, e) {
    return (t.lanes & e) !== 0
      ? !0
      : ((t = t.dependencies), !!(t !== null && Li(t)));
  }
  function lv(t, e, n) {
    switch (e.tag) {
      case 3:
        (tt(e, e.stateNode.containerInfo),
          cl(e, ae, t.memoizedState.cache),
          Ll());
        break;
      case 27:
      case 5:
        ht(e);
        break;
      case 4:
        tt(e, e.stateNode.containerInfo);
        break;
      case 10:
        cl(e, e.type, e.memoizedProps.value);
        break;
      case 31:
        if (e.memoizedState !== null) return ((e.flags |= 128), sr(e), null);
        break;
      case 13:
        var a = e.memoizedState;
        if (a !== null)
          return a.dehydrated !== null
            ? (fl(e), (e.flags |= 128), null)
            : (n & e.child.childLanes) !== 0
              ? jh(t, e, n)
              : (fl(e), (t = Hn(t, e, n)), t !== null ? t.sibling : null);
        fl(e);
        break;
      case 19:
        var c = (t.flags & 128) !== 0;
        if (
          ((a = (n & e.childLanes) !== 0),
          a || (xa(t, e, n, !1), (a = (n & e.childLanes) !== 0)),
          c)
        ) {
          if (a) return Bh(t, e, n);
          e.flags |= 128;
        }
        if (
          ((c = e.memoizedState),
          c !== null &&
            ((c.rendering = null), (c.tail = null), (c.lastEffect = null)),
          $(te, te.current),
          a)
        )
          break;
        return null;
      case 22:
        return ((e.lanes = 0), Mh(t, e, n, e.pendingProps));
      case 24:
        cl(e, ae, t.memoizedState.cache);
    }
    return Hn(t, e, n);
  }
  function Yh(t, e, n) {
    if (t !== null)
      if (t.memoizedProps !== e.pendingProps) ie = !0;
      else {
        if (!Br(t, n) && (e.flags & 128) === 0) return ((ie = !1), lv(t, e, n));
        ie = (t.flags & 131072) !== 0;
      }
    else ((ie = !1), Ct && (e.flags & 1048576) !== 0 && yd(e, gu, e.index));
    switch (((e.lanes = 0), e.tag)) {
      case 16:
        t: {
          var a = e.pendingProps;
          if (((t = kl(e.elementType)), (e.type = t), typeof t == "function"))
            Ls(t)
              ? ((a = Wl(t, a)), (e.tag = 1), (e = Rh(null, e, t, a, n)))
              : ((e.tag = 0), (e = Cr(null, e, t, a, n)));
          else {
            if (t != null) {
              var c = t.$$typeof;
              if (c === j) {
                ((e.tag = 11), (e = Ah(null, e, t, a, n)));
                break t;
              } else if (c === Q) {
                ((e.tag = 14), (e = Th(null, e, t, a, n)));
                break t;
              }
            }
            throw ((e = w(t) || t), Error(s(306, e, "")));
          }
        }
        return e;
      case 0:
        return Cr(t, e, e.type, e.pendingProps, n);
      case 1:
        return ((a = e.type), (c = Wl(a, e.pendingProps)), Rh(t, e, a, c, n));
      case 3:
        t: {
          if ((tt(e, e.stateNode.containerInfo), t === null))
            throw Error(s(387));
          a = e.pendingProps;
          var r = e.memoizedState;
          ((c = r.element), lr(t, e), _u(e, a, null, n));
          var h = e.memoizedState;
          if (
            ((a = h.cache),
            cl(e, ae, a),
            a !== r.cache && Fs(e, [ae], n, !0),
            Eu(),
            (a = h.element),
            r.isDehydrated)
          )
            if (
              ((r = { element: a, isDehydrated: !1, cache: h.cache }),
              (e.updateQueue.baseState = r),
              (e.memoizedState = r),
              e.flags & 256)
            ) {
              e = Hh(t, e, a, n);
              break t;
            } else if (a !== c) {
              ((c = Ke(Error(s(424)), e)), yu(c), (e = Hh(t, e, a, n)));
              break t;
            } else {
              switch (((t = e.stateNode.containerInfo), t.nodeType)) {
                case 9:
                  t = t.body;
                  break;
                default:
                  t = t.nodeName === "HTML" ? t.ownerDocument.body : t;
              }
              for (
                Zt = Ie(t.firstChild),
                  ye = e,
                  Ct = !0,
                  ul = null,
                  We = !0,
                  n = Md(e, null, a, n),
                  e.child = n;
                n;
              )
                ((n.flags = (n.flags & -3) | 4096), (n = n.sibling));
            }
          else {
            if ((Ll(), a === c)) {
              e = Hn(t, e, n);
              break t;
            }
            ve(t, e, a, n);
          }
          e = e.child;
        }
        return e;
      case 26:
        return (
          cc(t, e),
          t === null
            ? (n = Jm(e.type, null, e.pendingProps, null))
              ? (e.memoizedState = n)
              : Ct ||
                ((n = e.type),
                (t = e.pendingProps),
                (a = wc(ot.current).createElement(n)),
                (a[re] = e),
                (a[ge] = t),
                xe(a, n, t),
                le(a),
                (e.stateNode = a))
            : (e.memoizedState = Jm(
                e.type,
                t.memoizedProps,
                e.pendingProps,
                t.memoizedState,
              )),
          null
        );
      case 27:
        return (
          ht(e),
          t === null &&
            Ct &&
            ((a = e.stateNode = km(e.type, e.pendingProps, ot.current)),
            (ye = e),
            (We = !0),
            (c = Zt),
            bl(e.type) ? ((po = c), (Zt = Ie(a.firstChild))) : (Zt = c)),
          ve(t, e, e.pendingProps.children, n),
          cc(t, e),
          t === null && (e.flags |= 4194304),
          e.child
        );
      case 5:
        return (
          t === null &&
            Ct &&
            ((c = a = Zt) &&
              ((a = Rv(a, e.type, e.pendingProps, We)),
              a !== null
                ? ((e.stateNode = a),
                  (ye = e),
                  (Zt = Ie(a.firstChild)),
                  (We = !1),
                  (c = !0))
                : (c = !1)),
            c || il(e)),
          ht(e),
          (c = e.type),
          (r = e.pendingProps),
          (h = t !== null ? t.memoizedProps : null),
          (a = r.children),
          fo(c, r) ? (a = null) : h !== null && fo(c, h) && (e.flags |= 32),
          e.memoizedState !== null &&
            ((c = or(t, e, $p, null, null, n)), (Gu._currentValue = c)),
          cc(t, e),
          ve(t, e, a, n),
          e.child
        );
      case 6:
        return (
          t === null &&
            Ct &&
            ((t = n = Zt) &&
              ((n = Hv(n, e.pendingProps, We)),
              n !== null
                ? ((e.stateNode = n), (ye = e), (Zt = null), (t = !0))
                : (t = !1)),
            t || il(e)),
          null
        );
      case 13:
        return jh(t, e, n);
      case 4:
        return (
          tt(e, e.stateNode.containerInfo),
          (a = e.pendingProps),
          t === null ? (e.child = $l(e, null, a, n)) : ve(t, e, a, n),
          e.child
        );
      case 11:
        return Ah(t, e, e.type, e.pendingProps, n);
      case 7:
        return (ve(t, e, e.pendingProps, n), e.child);
      case 8:
        return (ve(t, e, e.pendingProps.children, n), e.child);
      case 12:
        return (ve(t, e, e.pendingProps.children, n), e.child);
      case 10:
        return (
          (a = e.pendingProps),
          cl(e, e.type, a.value),
          ve(t, e, a.children, n),
          e.child
        );
      case 9:
        return (
          (c = e.type._context),
          (a = e.pendingProps.children),
          Zl(e),
          (c = pe(c)),
          (a = a(c)),
          (e.flags |= 1),
          ve(t, e, a, n),
          e.child
        );
      case 14:
        return Th(t, e, e.type, e.pendingProps, n);
      case 15:
        return zh(t, e, e.type, e.pendingProps, n);
      case 19:
        return Bh(t, e, n);
      case 31:
        return nv(t, e, n);
      case 22:
        return Mh(t, e, n, e.pendingProps);
      case 24:
        return (
          Zl(e),
          (a = pe(ae)),
          t === null
            ? ((c = tr()),
              c === null &&
                ((c = Xt),
                (r = Is()),
                (c.pooledCache = r),
                r.refCount++,
                r !== null && (c.pooledCacheLanes |= n),
                (c = r)),
              (e.memoizedState = { parent: a, cache: c }),
              nr(e),
              cl(e, ae, c))
            : ((t.lanes & n) !== 0 && (lr(t, e), _u(e, null, null, n), Eu()),
              (c = t.memoizedState),
              (r = e.memoizedState),
              c.parent !== a
                ? ((c = { parent: a, cache: a }),
                  (e.memoizedState = c),
                  e.lanes === 0 &&
                    (e.memoizedState = e.updateQueue.baseState = c),
                  cl(e, ae, a))
                : ((a = r.cache),
                  cl(e, ae, a),
                  a !== c.cache && Fs(e, [ae], n, !0))),
          ve(t, e, e.pendingProps.children, n),
          e.child
        );
      case 29:
        throw e.pendingProps;
    }
    throw Error(s(156, e.tag));
  }
  function jn(t) {
    t.flags |= 4;
  }
  function Yr(t, e, n, a, c) {
    if (((e = (t.mode & 32) !== 0) && (e = !1), e)) {
      if (((t.flags |= 16777216), (c & 335544128) === c))
        if (t.stateNode.complete) t.flags |= 8192;
        else if (fm()) t.flags |= 8192;
        else throw ((Kl = ki), er);
    } else t.flags &= -16777217;
  }
  function qh(t, e) {
    if (e.type !== "stylesheet" || (e.state.loading & 4) !== 0)
      t.flags &= -16777217;
    else if (((t.flags |= 16777216), !t0(e)))
      if (fm()) t.flags |= 8192;
      else throw ((Kl = ki), er);
  }
  function rc(t, e) {
    (e !== null && (t.flags |= 4),
      t.flags & 16384 &&
        ((e = t.tag !== 22 ? gi() : 536870912), (t.lanes |= e), (Da |= e)));
  }
  function Mu(t, e) {
    if (!Ct)
      switch (t.tailMode) {
        case "hidden":
          e = t.tail;
          for (var n = null; e !== null; )
            (e.alternate !== null && (n = e), (e = e.sibling));
          n === null ? (t.tail = null) : (n.sibling = null);
          break;
        case "collapsed":
          n = t.tail;
          for (var a = null; n !== null; )
            (n.alternate !== null && (a = n), (n = n.sibling));
          a === null
            ? e || t.tail === null
              ? (t.tail = null)
              : (t.tail.sibling = null)
            : (a.sibling = null);
      }
  }
  function Qt(t) {
    var e = t.alternate !== null && t.alternate.child === t.child,
      n = 0,
      a = 0;
    if (e)
      for (var c = t.child; c !== null; )
        ((n |= c.lanes | c.childLanes),
          (a |= c.subtreeFlags & 65011712),
          (a |= c.flags & 65011712),
          (c.return = t),
          (c = c.sibling));
    else
      for (c = t.child; c !== null; )
        ((n |= c.lanes | c.childLanes),
          (a |= c.subtreeFlags),
          (a |= c.flags),
          (c.return = t),
          (c = c.sibling));
    return ((t.subtreeFlags |= a), (t.childLanes = n), e);
  }
  function av(t, e, n) {
    var a = e.pendingProps;
    switch ((ks(e), e.tag)) {
      case 16:
      case 15:
      case 0:
      case 11:
      case 7:
      case 8:
      case 12:
      case 9:
      case 14:
        return (Qt(e), null);
      case 1:
        return (Qt(e), null);
      case 3:
        return (
          (n = e.stateNode),
          (a = null),
          t !== null && (a = t.memoizedState.cache),
          e.memoizedState.cache !== a && (e.flags |= 2048),
          Dn(ae),
          ft(),
          n.pendingContext &&
            ((n.context = n.pendingContext), (n.pendingContext = null)),
          (t === null || t.child === null) &&
            (va(e)
              ? jn(e)
              : t === null ||
                (t.memoizedState.isDehydrated && (e.flags & 256) === 0) ||
                ((e.flags |= 1024), $s())),
          Qt(e),
          null
        );
      case 26:
        var c = e.type,
          r = e.memoizedState;
        return (
          t === null
            ? (jn(e),
              r !== null ? (Qt(e), qh(e, r)) : (Qt(e), Yr(e, c, null, a, n)))
            : r
              ? r !== t.memoizedState
                ? (jn(e), Qt(e), qh(e, r))
                : (Qt(e), (e.flags &= -16777217))
              : ((t = t.memoizedProps),
                t !== a && jn(e),
                Qt(e),
                Yr(e, c, t, a, n)),
          null
        );
      case 27:
        if (
          (bt(e),
          (n = ot.current),
          (c = e.type),
          t !== null && e.stateNode != null)
        )
          t.memoizedProps !== a && jn(e);
        else {
          if (!a) {
            if (e.stateNode === null) throw Error(s(166));
            return (Qt(e), null);
          }
          ((t = F.current),
            va(e) ? vd(e) : ((t = km(c, a, n)), (e.stateNode = t), jn(e)));
        }
        return (Qt(e), null);
      case 5:
        if ((bt(e), (c = e.type), t !== null && e.stateNode != null))
          t.memoizedProps !== a && jn(e);
        else {
          if (!a) {
            if (e.stateNode === null) throw Error(s(166));
            return (Qt(e), null);
          }
          if (((r = F.current), va(e))) vd(e);
          else {
            var h = wc(ot.current);
            switch (r) {
              case 1:
                r = h.createElementNS("http://www.w3.org/2000/svg", c);
                break;
              case 2:
                r = h.createElementNS("http://www.w3.org/1998/Math/MathML", c);
                break;
              default:
                switch (c) {
                  case "svg":
                    r = h.createElementNS("http://www.w3.org/2000/svg", c);
                    break;
                  case "math":
                    r = h.createElementNS(
                      "http://www.w3.org/1998/Math/MathML",
                      c,
                    );
                    break;
                  case "script":
                    ((r = h.createElement("div")),
                      (r.innerHTML = "<script><\/script>"),
                      (r = r.removeChild(r.firstChild)));
                    break;
                  case "select":
                    ((r =
                      typeof a.is == "string"
                        ? h.createElement("select", { is: a.is })
                        : h.createElement("select")),
                      a.multiple
                        ? (r.multiple = !0)
                        : a.size && (r.size = a.size));
                    break;
                  default:
                    r =
                      typeof a.is == "string"
                        ? h.createElement(c, { is: a.is })
                        : h.createElement(c);
                }
            }
            ((r[re] = e), (r[ge] = a));
            t: for (h = e.child; h !== null; ) {
              if (h.tag === 5 || h.tag === 6) r.appendChild(h.stateNode);
              else if (h.tag !== 4 && h.tag !== 27 && h.child !== null) {
                ((h.child.return = h), (h = h.child));
                continue;
              }
              if (h === e) break t;
              for (; h.sibling === null; ) {
                if (h.return === null || h.return === e) break t;
                h = h.return;
              }
              ((h.sibling.return = h.return), (h = h.sibling));
            }
            e.stateNode = r;
            t: switch ((xe(r, c, a), c)) {
              case "button":
              case "input":
              case "select":
              case "textarea":
                a = !!a.autoFocus;
                break t;
              case "img":
                a = !0;
                break t;
              default:
                a = !1;
            }
            a && jn(e);
          }
        }
        return (
          Qt(e),
          Yr(e, e.type, t === null ? null : t.memoizedProps, e.pendingProps, n),
          null
        );
      case 6:
        if (t && e.stateNode != null) t.memoizedProps !== a && jn(e);
        else {
          if (typeof a != "string" && e.stateNode === null) throw Error(s(166));
          if (((t = ot.current), va(e))) {
            if (
              ((t = e.stateNode),
              (n = e.memoizedProps),
              (a = null),
              (c = ye),
              c !== null)
            )
              switch (c.tag) {
                case 27:
                case 5:
                  a = c.memoizedProps;
              }
            ((t[re] = e),
              (t = !!(
                t.nodeValue === n ||
                (a !== null && a.suppressHydrationWarning === !0) ||
                jm(t.nodeValue, n)
              )),
              t || il(e, !0));
          } else
            ((t = wc(t).createTextNode(a)), (t[re] = e), (e.stateNode = t));
        }
        return (Qt(e), null);
      case 31:
        if (((n = e.memoizedState), t === null || t.memoizedState !== null)) {
          if (((a = va(e)), n !== null)) {
            if (t === null) {
              if (!a) throw Error(s(318));
              if (
                ((t = e.memoizedState),
                (t = t !== null ? t.dehydrated : null),
                !t)
              )
                throw Error(s(557));
              t[re] = e;
            } else
              (Ll(),
                (e.flags & 128) === 0 && (e.memoizedState = null),
                (e.flags |= 4));
            (Qt(e), (t = !1));
          } else
            ((n = $s()),
              t !== null &&
                t.memoizedState !== null &&
                (t.memoizedState.hydrationErrors = n),
              (t = !0));
          if (!t) return e.flags & 256 ? (Ye(e), e) : (Ye(e), null);
          if ((e.flags & 128) !== 0) throw Error(s(558));
        }
        return (Qt(e), null);
      case 13:
        if (
          ((a = e.memoizedState),
          t === null ||
            (t.memoizedState !== null && t.memoizedState.dehydrated !== null))
        ) {
          if (((c = va(e)), a !== null && a.dehydrated !== null)) {
            if (t === null) {
              if (!c) throw Error(s(318));
              if (
                ((c = e.memoizedState),
                (c = c !== null ? c.dehydrated : null),
                !c)
              )
                throw Error(s(317));
              c[re] = e;
            } else
              (Ll(),
                (e.flags & 128) === 0 && (e.memoizedState = null),
                (e.flags |= 4));
            (Qt(e), (c = !1));
          } else
            ((c = $s()),
              t !== null &&
                t.memoizedState !== null &&
                (t.memoizedState.hydrationErrors = c),
              (c = !0));
          if (!c) return e.flags & 256 ? (Ye(e), e) : (Ye(e), null);
        }
        return (
          Ye(e),
          (e.flags & 128) !== 0
            ? ((e.lanes = n), e)
            : ((n = a !== null),
              (t = t !== null && t.memoizedState !== null),
              n &&
                ((a = e.child),
                (c = null),
                a.alternate !== null &&
                  a.alternate.memoizedState !== null &&
                  a.alternate.memoizedState.cachePool !== null &&
                  (c = a.alternate.memoizedState.cachePool.pool),
                (r = null),
                a.memoizedState !== null &&
                  a.memoizedState.cachePool !== null &&
                  (r = a.memoizedState.cachePool.pool),
                r !== c && (a.flags |= 2048)),
              n !== t && n && (e.child.flags |= 8192),
              rc(e, e.updateQueue),
              Qt(e),
              null)
        );
      case 4:
        return (ft(), t === null && io(e.stateNode.containerInfo), Qt(e), null);
      case 10:
        return (Dn(e.type), Qt(e), null);
      case 19:
        if ((M(te), (a = e.memoizedState), a === null)) return (Qt(e), null);
        if (((c = (e.flags & 128) !== 0), (r = a.rendering), r === null))
          if (c) Mu(a, !1);
          else {
            if (Wt !== 0 || (t !== null && (t.flags & 128) !== 0))
              for (t = e.child; t !== null; ) {
                if (((r = Wi(t)), r !== null)) {
                  for (
                    e.flags |= 128,
                      Mu(a, !1),
                      t = r.updateQueue,
                      e.updateQueue = t,
                      rc(e, t),
                      e.subtreeFlags = 0,
                      t = n,
                      n = e.child;
                    n !== null;
                  )
                    (hd(n, t), (n = n.sibling));
                  return (
                    $(te, (te.current & 1) | 2),
                    Ct && Mn(e, a.treeForkCount),
                    e.child
                  );
                }
                t = t.sibling;
              }
            a.tail !== null &&
              Kt() > mc &&
              ((e.flags |= 128), (c = !0), Mu(a, !1), (e.lanes = 4194304));
          }
        else {
          if (!c)
            if (((t = Wi(r)), t !== null)) {
              if (
                ((e.flags |= 128),
                (c = !0),
                (t = t.updateQueue),
                (e.updateQueue = t),
                rc(e, t),
                Mu(a, !0),
                a.tail === null &&
                  a.tailMode === "hidden" &&
                  !r.alternate &&
                  !Ct)
              )
                return (Qt(e), null);
            } else
              2 * Kt() - a.renderingStartTime > mc &&
                n !== 536870912 &&
                ((e.flags |= 128), (c = !0), Mu(a, !1), (e.lanes = 4194304));
          a.isBackwards
            ? ((r.sibling = e.child), (e.child = r))
            : ((t = a.last),
              t !== null ? (t.sibling = r) : (e.child = r),
              (a.last = r));
        }
        return a.tail !== null
          ? ((t = a.tail),
            (a.rendering = t),
            (a.tail = t.sibling),
            (a.renderingStartTime = Kt()),
            (t.sibling = null),
            (n = te.current),
            $(te, c ? (n & 1) | 2 : n & 1),
            Ct && Mn(e, a.treeForkCount),
            t)
          : (Qt(e), null);
      case 22:
      case 23:
        return (
          Ye(e),
          cr(),
          (a = e.memoizedState !== null),
          t !== null
            ? (t.memoizedState !== null) !== a && (e.flags |= 8192)
            : a && (e.flags |= 8192),
          a
            ? (n & 536870912) !== 0 &&
              (e.flags & 128) === 0 &&
              (Qt(e), e.subtreeFlags & 6 && (e.flags |= 8192))
            : Qt(e),
          (n = e.updateQueue),
          n !== null && rc(e, n.retryQueue),
          (n = null),
          t !== null &&
            t.memoizedState !== null &&
            t.memoizedState.cachePool !== null &&
            (n = t.memoizedState.cachePool.pool),
          (a = null),
          e.memoizedState !== null &&
            e.memoizedState.cachePool !== null &&
            (a = e.memoizedState.cachePool.pool),
          a !== n && (e.flags |= 2048),
          t !== null && M(Ql),
          null
        );
      case 24:
        return (
          (n = null),
          t !== null && (n = t.memoizedState.cache),
          e.memoizedState.cache !== n && (e.flags |= 2048),
          Dn(ae),
          Qt(e),
          null
        );
      case 25:
        return null;
      case 30:
        return null;
    }
    throw Error(s(156, e.tag));
  }
  function uv(t, e) {
    switch ((ks(e), e.tag)) {
      case 1:
        return (
          (t = e.flags),
          t & 65536 ? ((e.flags = (t & -65537) | 128), e) : null
        );
      case 3:
        return (
          Dn(ae),
          ft(),
          (t = e.flags),
          (t & 65536) !== 0 && (t & 128) === 0
            ? ((e.flags = (t & -65537) | 128), e)
            : null
        );
      case 26:
      case 27:
      case 5:
        return (bt(e), null);
      case 31:
        if (e.memoizedState !== null) {
          if ((Ye(e), e.alternate === null)) throw Error(s(340));
          Ll();
        }
        return (
          (t = e.flags),
          t & 65536 ? ((e.flags = (t & -65537) | 128), e) : null
        );
      case 13:
        if (
          (Ye(e), (t = e.memoizedState), t !== null && t.dehydrated !== null)
        ) {
          if (e.alternate === null) throw Error(s(340));
          Ll();
        }
        return (
          (t = e.flags),
          t & 65536 ? ((e.flags = (t & -65537) | 128), e) : null
        );
      case 19:
        return (M(te), null);
      case 4:
        return (ft(), null);
      case 10:
        return (Dn(e.type), null);
      case 22:
      case 23:
        return (
          Ye(e),
          cr(),
          t !== null && M(Ql),
          (t = e.flags),
          t & 65536 ? ((e.flags = (t & -65537) | 128), e) : null
        );
      case 24:
        return (Dn(ae), null);
      case 25:
        return null;
      default:
        return null;
    }
  }
  function Xh(t, e) {
    switch ((ks(e), e.tag)) {
      case 3:
        (Dn(ae), ft());
        break;
      case 26:
      case 27:
      case 5:
        bt(e);
        break;
      case 4:
        ft();
        break;
      case 31:
        e.memoizedState !== null && Ye(e);
        break;
      case 13:
        Ye(e);
        break;
      case 19:
        M(te);
        break;
      case 10:
        Dn(e.type);
        break;
      case 22:
      case 23:
        (Ye(e), cr(), t !== null && M(Ql));
        break;
      case 24:
        Dn(ae);
    }
  }
  function Cu(t, e) {
    try {
      var n = e.updateQueue,
        a = n !== null ? n.lastEffect : null;
      if (a !== null) {
        var c = a.next;
        n = c;
        do {
          if ((n.tag & t) === t) {
            a = void 0;
            var r = n.create,
              h = n.inst;
            ((a = r()), (h.destroy = a));
          }
          n = n.next;
        } while (n !== c);
      }
    } catch (v) {
      Ut(e, e.return, v);
    }
  }
  function hl(t, e, n) {
    try {
      var a = e.updateQueue,
        c = a !== null ? a.lastEffect : null;
      if (c !== null) {
        var r = c.next;
        a = r;
        do {
          if ((a.tag & t) === t) {
            var h = a.inst,
              v = h.destroy;
            if (v !== void 0) {
              ((h.destroy = void 0), (c = e));
              var T = n,
                X = v;
              try {
                X();
              } catch (P) {
                Ut(c, T, P);
              }
            }
          }
          a = a.next;
        } while (a !== r);
      }
    } catch (P) {
      Ut(e, e.return, P);
    }
  }
  function Vh(t) {
    var e = t.updateQueue;
    if (e !== null) {
      var n = t.stateNode;
      try {
        Dd(e, n);
      } catch (a) {
        Ut(t, t.return, a);
      }
    }
  }
  function Lh(t, e, n) {
    ((n.props = Wl(t.type, t.memoizedProps)), (n.state = t.memoizedState));
    try {
      n.componentWillUnmount();
    } catch (a) {
      Ut(t, e, a);
    }
  }
  function Du(t, e) {
    try {
      var n = t.ref;
      if (n !== null) {
        switch (t.tag) {
          case 26:
          case 27:
          case 5:
            var a = t.stateNode;
            break;
          case 30:
            a = t.stateNode;
            break;
          default:
            a = t.stateNode;
        }
        typeof n == "function" ? (t.refCleanup = n(a)) : (n.current = a);
      }
    } catch (c) {
      Ut(t, e, c);
    }
  }
  function pn(t, e) {
    var n = t.ref,
      a = t.refCleanup;
    if (n !== null)
      if (typeof a == "function")
        try {
          a();
        } catch (c) {
          Ut(t, e, c);
        } finally {
          ((t.refCleanup = null),
            (t = t.alternate),
            t != null && (t.refCleanup = null));
        }
      else if (typeof n == "function")
        try {
          n(null);
        } catch (c) {
          Ut(t, e, c);
        }
      else n.current = null;
  }
  function Gh(t) {
    var e = t.type,
      n = t.memoizedProps,
      a = t.stateNode;
    try {
      t: switch (e) {
        case "button":
        case "input":
        case "select":
        case "textarea":
          n.autoFocus && a.focus();
          break t;
        case "img":
          n.src ? (a.src = n.src) : n.srcSet && (a.srcset = n.srcSet);
      }
    } catch (c) {
      Ut(t, t.return, c);
    }
  }
  function qr(t, e, n) {
    try {
      var a = t.stateNode;
      (Tv(a, t.type, n, e), (a[ge] = e));
    } catch (c) {
      Ut(t, t.return, c);
    }
  }
  function Zh(t) {
    return (
      t.tag === 5 ||
      t.tag === 3 ||
      t.tag === 26 ||
      (t.tag === 27 && bl(t.type)) ||
      t.tag === 4
    );
  }
  function Xr(t) {
    t: for (;;) {
      for (; t.sibling === null; ) {
        if (t.return === null || Zh(t.return)) return null;
        t = t.return;
      }
      for (
        t.sibling.return = t.return, t = t.sibling;
        t.tag !== 5 && t.tag !== 6 && t.tag !== 18;
      ) {
        if (
          (t.tag === 27 && bl(t.type)) ||
          t.flags & 2 ||
          t.child === null ||
          t.tag === 4
        )
          continue t;
        ((t.child.return = t), (t = t.child));
      }
      if (!(t.flags & 2)) return t.stateNode;
    }
  }
  function Vr(t, e, n) {
    var a = t.tag;
    if (a === 5 || a === 6)
      ((t = t.stateNode),
        e
          ? (n.nodeType === 9
              ? n.body
              : n.nodeName === "HTML"
                ? n.ownerDocument.body
                : n
            ).insertBefore(t, e)
          : ((e =
              n.nodeType === 9
                ? n.body
                : n.nodeName === "HTML"
                  ? n.ownerDocument.body
                  : n),
            e.appendChild(t),
            (n = n._reactRootContainer),
            n != null || e.onclick !== null || (e.onclick = An)));
    else if (
      a !== 4 &&
      (a === 27 && bl(t.type) && ((n = t.stateNode), (e = null)),
      (t = t.child),
      t !== null)
    )
      for (Vr(t, e, n), t = t.sibling; t !== null; )
        (Vr(t, e, n), (t = t.sibling));
  }
  function oc(t, e, n) {
    var a = t.tag;
    if (a === 5 || a === 6)
      ((t = t.stateNode), e ? n.insertBefore(t, e) : n.appendChild(t));
    else if (
      a !== 4 &&
      (a === 27 && bl(t.type) && (n = t.stateNode), (t = t.child), t !== null)
    )
      for (oc(t, e, n), t = t.sibling; t !== null; )
        (oc(t, e, n), (t = t.sibling));
  }
  function Qh(t) {
    var e = t.stateNode,
      n = t.memoizedProps;
    try {
      for (var a = t.type, c = e.attributes; c.length; )
        e.removeAttributeNode(c[0]);
      (xe(e, a, n), (e[re] = t), (e[ge] = n));
    } catch (r) {
      Ut(t, t.return, r);
    }
  }
  var Un = !1,
    ce = !1,
    Lr = !1,
    kh = typeof WeakSet == "function" ? WeakSet : Set,
    he = null;
  function iv(t, e) {
    if (((t = t.containerInfo), (ro = Dc), (t = ad(t)), js(t))) {
      if ("selectionStart" in t)
        var n = { start: t.selectionStart, end: t.selectionEnd };
      else
        t: {
          n = ((n = t.ownerDocument) && n.defaultView) || window;
          var a = n.getSelection && n.getSelection();
          if (a && a.rangeCount !== 0) {
            n = a.anchorNode;
            var c = a.anchorOffset,
              r = a.focusNode;
            a = a.focusOffset;
            try {
              (n.nodeType, r.nodeType);
            } catch {
              n = null;
              break t;
            }
            var h = 0,
              v = -1,
              T = -1,
              X = 0,
              P = 0,
              lt = t,
              V = null;
            e: for (;;) {
              for (
                var K;
                lt !== n || (c !== 0 && lt.nodeType !== 3) || (v = h + c),
                  lt !== r || (a !== 0 && lt.nodeType !== 3) || (T = h + a),
                  lt.nodeType === 3 && (h += lt.nodeValue.length),
                  (K = lt.firstChild) !== null;
              )
                ((V = lt), (lt = K));
              for (;;) {
                if (lt === t) break e;
                if (
                  (V === n && ++X === c && (v = h),
                  V === r && ++P === a && (T = h),
                  (K = lt.nextSibling) !== null)
                )
                  break;
                ((lt = V), (V = lt.parentNode));
              }
              lt = K;
            }
            n = v === -1 || T === -1 ? null : { start: v, end: T };
          } else n = null;
        }
      n = n || { start: 0, end: 0 };
    } else n = null;
    for (
      oo = { focusedElem: t, selectionRange: n }, Dc = !1, he = e;
      he !== null;
    )
      if (
        ((e = he), (t = e.child), (e.subtreeFlags & 1028) !== 0 && t !== null)
      )
        ((t.return = e), (he = t));
      else
        for (; he !== null; ) {
          switch (((e = he), (r = e.alternate), (t = e.flags), e.tag)) {
            case 0:
              if (
                (t & 4) !== 0 &&
                ((t = e.updateQueue),
                (t = t !== null ? t.events : null),
                t !== null)
              )
                for (n = 0; n < t.length; n++)
                  ((c = t[n]), (c.ref.impl = c.nextImpl));
              break;
            case 11:
            case 15:
              break;
            case 1:
              if ((t & 1024) !== 0 && r !== null) {
                ((t = void 0),
                  (n = e),
                  (c = r.memoizedProps),
                  (r = r.memoizedState),
                  (a = n.stateNode));
                try {
                  var mt = Wl(n.type, c);
                  ((t = a.getSnapshotBeforeUpdate(mt, r)),
                    (a.__reactInternalSnapshotBeforeUpdate = t));
                } catch (xt) {
                  Ut(n, n.return, xt);
                }
              }
              break;
            case 3:
              if ((t & 1024) !== 0) {
                if (
                  ((t = e.stateNode.containerInfo), (n = t.nodeType), n === 9)
                )
                  mo(t);
                else if (n === 1)
                  switch (t.nodeName) {
                    case "HEAD":
                    case "HTML":
                    case "BODY":
                      mo(t);
                      break;
                    default:
                      t.textContent = "";
                  }
              }
              break;
            case 5:
            case 26:
            case 27:
            case 6:
            case 4:
            case 17:
              break;
            default:
              if ((t & 1024) !== 0) throw Error(s(163));
          }
          if (((t = e.sibling), t !== null)) {
            ((t.return = e.return), (he = t));
            break;
          }
          he = e.return;
        }
  }
  function Kh(t, e, n) {
    var a = n.flags;
    switch (n.tag) {
      case 0:
      case 11:
      case 15:
        (Yn(t, n), a & 4 && Cu(5, n));
        break;
      case 1:
        if ((Yn(t, n), a & 4))
          if (((t = n.stateNode), e === null))
            try {
              t.componentDidMount();
            } catch (h) {
              Ut(n, n.return, h);
            }
          else {
            var c = Wl(n.type, e.memoizedProps);
            e = e.memoizedState;
            try {
              t.componentDidUpdate(c, e, t.__reactInternalSnapshotBeforeUpdate);
            } catch (h) {
              Ut(n, n.return, h);
            }
          }
        (a & 64 && Vh(n), a & 512 && Du(n, n.return));
        break;
      case 3:
        if ((Yn(t, n), a & 64 && ((t = n.updateQueue), t !== null))) {
          if (((e = null), n.child !== null))
            switch (n.child.tag) {
              case 27:
              case 5:
                e = n.child.stateNode;
                break;
              case 1:
                e = n.child.stateNode;
            }
          try {
            Dd(t, e);
          } catch (h) {
            Ut(n, n.return, h);
          }
        }
        break;
      case 27:
        e === null && a & 4 && Qh(n);
      case 26:
      case 5:
        (Yn(t, n), e === null && a & 4 && Gh(n), a & 512 && Du(n, n.return));
        break;
      case 12:
        Yn(t, n);
        break;
      case 31:
        (Yn(t, n), a & 4 && Wh(t, n));
        break;
      case 13:
        (Yn(t, n),
          a & 4 && Fh(t, n),
          a & 64 &&
            ((t = n.memoizedState),
            t !== null &&
              ((t = t.dehydrated),
              t !== null && ((n = gv.bind(null, n)), jv(t, n)))));
        break;
      case 22:
        if (((a = n.memoizedState !== null || Un), !a)) {
          ((e = (e !== null && e.memoizedState !== null) || ce), (c = Un));
          var r = ce;
          ((Un = a),
            (ce = e) && !r ? qn(t, n, (n.subtreeFlags & 8772) !== 0) : Yn(t, n),
            (Un = c),
            (ce = r));
        }
        break;
      case 30:
        break;
      default:
        Yn(t, n);
    }
  }
  function $h(t) {
    var e = t.alternate;
    (e !== null && ((t.alternate = null), $h(e)),
      (t.child = null),
      (t.deletions = null),
      (t.sibling = null),
      t.tag === 5 && ((e = t.stateNode), e !== null && au(e)),
      (t.stateNode = null),
      (t.return = null),
      (t.dependencies = null),
      (t.memoizedProps = null),
      (t.memoizedState = null),
      (t.pendingProps = null),
      (t.stateNode = null),
      (t.updateQueue = null));
  }
  var kt = null,
    Ae = !1;
  function Bn(t, e, n) {
    for (n = n.child; n !== null; ) (Jh(t, e, n), (n = n.sibling));
  }
  function Jh(t, e, n) {
    if (Pt && typeof Pt.onCommitFiberUnmount == "function")
      try {
        Pt.onCommitFiberUnmount(He, n);
      } catch {}
    switch (n.tag) {
      case 26:
        (ce || pn(n, e),
          Bn(t, e, n),
          n.memoizedState
            ? n.memoizedState.count--
            : n.stateNode && ((n = n.stateNode), n.parentNode.removeChild(n)));
        break;
      case 27:
        ce || pn(n, e);
        var a = kt,
          c = Ae;
        (bl(n.type) && ((kt = n.stateNode), (Ae = !1)),
          Bn(t, e, n),
          Xu(n.stateNode),
          (kt = a),
          (Ae = c));
        break;
      case 5:
        ce || pn(n, e);
      case 6:
        if (
          ((a = kt),
          (c = Ae),
          (kt = null),
          Bn(t, e, n),
          (kt = a),
          (Ae = c),
          kt !== null)
        )
          if (Ae)
            try {
              (kt.nodeType === 9
                ? kt.body
                : kt.nodeName === "HTML"
                  ? kt.ownerDocument.body
                  : kt
              ).removeChild(n.stateNode);
            } catch (r) {
              Ut(n, e, r);
            }
          else
            try {
              kt.removeChild(n.stateNode);
            } catch (r) {
              Ut(n, e, r);
            }
        break;
      case 18:
        kt !== null &&
          (Ae
            ? ((t = kt),
              Vm(
                t.nodeType === 9
                  ? t.body
                  : t.nodeName === "HTML"
                    ? t.ownerDocument.body
                    : t,
                n.stateNode,
              ),
              qa(t))
            : Vm(kt, n.stateNode));
        break;
      case 4:
        ((a = kt),
          (c = Ae),
          (kt = n.stateNode.containerInfo),
          (Ae = !0),
          Bn(t, e, n),
          (kt = a),
          (Ae = c));
        break;
      case 0:
      case 11:
      case 14:
      case 15:
        (hl(2, n, e), ce || hl(4, n, e), Bn(t, e, n));
        break;
      case 1:
        (ce ||
          (pn(n, e),
          (a = n.stateNode),
          typeof a.componentWillUnmount == "function" && Lh(n, e, a)),
          Bn(t, e, n));
        break;
      case 21:
        Bn(t, e, n);
        break;
      case 22:
        ((ce = (a = ce) || n.memoizedState !== null), Bn(t, e, n), (ce = a));
        break;
      default:
        Bn(t, e, n);
    }
  }
  function Wh(t, e) {
    if (
      e.memoizedState === null &&
      ((t = e.alternate), t !== null && ((t = t.memoizedState), t !== null))
    ) {
      t = t.dehydrated;
      try {
        qa(t);
      } catch (n) {
        Ut(e, e.return, n);
      }
    }
  }
  function Fh(t, e) {
    if (
      e.memoizedState === null &&
      ((t = e.alternate),
      t !== null &&
        ((t = t.memoizedState), t !== null && ((t = t.dehydrated), t !== null)))
    )
      try {
        qa(t);
      } catch (n) {
        Ut(e, e.return, n);
      }
  }
  function cv(t) {
    switch (t.tag) {
      case 31:
      case 13:
      case 19:
        var e = t.stateNode;
        return (e === null && (e = t.stateNode = new kh()), e);
      case 22:
        return (
          (t = t.stateNode),
          (e = t._retryCache),
          e === null && (e = t._retryCache = new kh()),
          e
        );
      default:
        throw Error(s(435, t.tag));
    }
  }
  function fc(t, e) {
    var n = cv(t);
    e.forEach(function (a) {
      if (!n.has(a)) {
        n.add(a);
        var c = yv.bind(null, t, a);
        a.then(c, c);
      }
    });
  }
  function Te(t, e) {
    var n = e.deletions;
    if (n !== null)
      for (var a = 0; a < n.length; a++) {
        var c = n[a],
          r = t,
          h = e,
          v = h;
        t: for (; v !== null; ) {
          switch (v.tag) {
            case 27:
              if (bl(v.type)) {
                ((kt = v.stateNode), (Ae = !1));
                break t;
              }
              break;
            case 5:
              ((kt = v.stateNode), (Ae = !1));
              break t;
            case 3:
            case 4:
              ((kt = v.stateNode.containerInfo), (Ae = !0));
              break t;
          }
          v = v.return;
        }
        if (kt === null) throw Error(s(160));
        (Jh(r, h, c),
          (kt = null),
          (Ae = !1),
          (r = c.alternate),
          r !== null && (r.return = null),
          (c.return = null));
      }
    if (e.subtreeFlags & 13886)
      for (e = e.child; e !== null; ) (Ih(e, t), (e = e.sibling));
  }
  var an = null;
  function Ih(t, e) {
    var n = t.alternate,
      a = t.flags;
    switch (t.tag) {
      case 0:
      case 11:
      case 14:
      case 15:
        (Te(e, t),
          ze(t),
          a & 4 && (hl(3, t, t.return), Cu(3, t), hl(5, t, t.return)));
        break;
      case 1:
        (Te(e, t),
          ze(t),
          a & 512 && (ce || n === null || pn(n, n.return)),
          a & 64 &&
            Un &&
            ((t = t.updateQueue),
            t !== null &&
              ((a = t.callbacks),
              a !== null &&
                ((n = t.shared.hiddenCallbacks),
                (t.shared.hiddenCallbacks = n === null ? a : n.concat(a))))));
        break;
      case 26:
        var c = an;
        if (
          (Te(e, t),
          ze(t),
          a & 512 && (ce || n === null || pn(n, n.return)),
          a & 4)
        ) {
          var r = n !== null ? n.memoizedState : null;
          if (((a = t.memoizedState), n === null))
            if (a === null)
              if (t.stateNode === null) {
                t: {
                  ((a = t.type),
                    (n = t.memoizedProps),
                    (c = c.ownerDocument || c));
                  e: switch (a) {
                    case "title":
                      ((r = c.getElementsByTagName("title")[0]),
                        (!r ||
                          r[jl] ||
                          r[re] ||
                          r.namespaceURI === "http://www.w3.org/2000/svg" ||
                          r.hasAttribute("itemprop")) &&
                          ((r = c.createElement(a)),
                          c.head.insertBefore(
                            r,
                            c.querySelector("head > title"),
                          )),
                        xe(r, a, n),
                        (r[re] = t),
                        le(r),
                        (a = r));
                      break t;
                    case "link":
                      var h = Im("link", "href", c).get(a + (n.href || ""));
                      if (h) {
                        for (var v = 0; v < h.length; v++)
                          if (
                            ((r = h[v]),
                            r.getAttribute("href") ===
                              (n.href == null || n.href === ""
                                ? null
                                : n.href) &&
                              r.getAttribute("rel") ===
                                (n.rel == null ? null : n.rel) &&
                              r.getAttribute("title") ===
                                (n.title == null ? null : n.title) &&
                              r.getAttribute("crossorigin") ===
                                (n.crossOrigin == null ? null : n.crossOrigin))
                          ) {
                            h.splice(v, 1);
                            break e;
                          }
                      }
                      ((r = c.createElement(a)),
                        xe(r, a, n),
                        c.head.appendChild(r));
                      break;
                    case "meta":
                      if (
                        (h = Im("meta", "content", c).get(
                          a + (n.content || ""),
                        ))
                      ) {
                        for (v = 0; v < h.length; v++)
                          if (
                            ((r = h[v]),
                            r.getAttribute("content") ===
                              (n.content == null ? null : "" + n.content) &&
                              r.getAttribute("name") ===
                                (n.name == null ? null : n.name) &&
                              r.getAttribute("property") ===
                                (n.property == null ? null : n.property) &&
                              r.getAttribute("http-equiv") ===
                                (n.httpEquiv == null ? null : n.httpEquiv) &&
                              r.getAttribute("charset") ===
                                (n.charSet == null ? null : n.charSet))
                          ) {
                            h.splice(v, 1);
                            break e;
                          }
                      }
                      ((r = c.createElement(a)),
                        xe(r, a, n),
                        c.head.appendChild(r));
                      break;
                    default:
                      throw Error(s(468, a));
                  }
                  ((r[re] = t), le(r), (a = r));
                }
                t.stateNode = a;
              } else Pm(c, t.type, t.stateNode);
            else t.stateNode = Fm(c, a, t.memoizedProps);
          else
            r !== a
              ? (r === null
                  ? n.stateNode !== null &&
                    ((n = n.stateNode), n.parentNode.removeChild(n))
                  : r.count--,
                a === null
                  ? Pm(c, t.type, t.stateNode)
                  : Fm(c, a, t.memoizedProps))
              : a === null &&
                t.stateNode !== null &&
                qr(t, t.memoizedProps, n.memoizedProps);
        }
        break;
      case 27:
        (Te(e, t),
          ze(t),
          a & 512 && (ce || n === null || pn(n, n.return)),
          n !== null && a & 4 && qr(t, t.memoizedProps, n.memoizedProps));
        break;
      case 5:
        if (
          (Te(e, t),
          ze(t),
          a & 512 && (ce || n === null || pn(n, n.return)),
          t.flags & 32)
        ) {
          c = t.stateNode;
          try {
            sa(c, "");
          } catch (mt) {
            Ut(t, t.return, mt);
          }
        }
        (a & 4 &&
          t.stateNode != null &&
          ((c = t.memoizedProps), qr(t, c, n !== null ? n.memoizedProps : c)),
          a & 1024 && (Lr = !0));
        break;
      case 6:
        if ((Te(e, t), ze(t), a & 4)) {
          if (t.stateNode === null) throw Error(s(162));
          ((a = t.memoizedProps), (n = t.stateNode));
          try {
            n.nodeValue = a;
          } catch (mt) {
            Ut(t, t.return, mt);
          }
        }
        break;
      case 3:
        if (
          ((Tc = null),
          (c = an),
          (an = Nc(e.containerInfo)),
          Te(e, t),
          (an = c),
          ze(t),
          a & 4 && n !== null && n.memoizedState.isDehydrated)
        )
          try {
            qa(e.containerInfo);
          } catch (mt) {
            Ut(t, t.return, mt);
          }
        Lr && ((Lr = !1), Ph(t));
        break;
      case 4:
        ((a = an),
          (an = Nc(t.stateNode.containerInfo)),
          Te(e, t),
          ze(t),
          (an = a));
        break;
      case 12:
        (Te(e, t), ze(t));
        break;
      case 31:
        (Te(e, t),
          ze(t),
          a & 4 &&
            ((a = t.updateQueue),
            a !== null && ((t.updateQueue = null), fc(t, a))));
        break;
      case 13:
        (Te(e, t),
          ze(t),
          t.child.flags & 8192 &&
            (t.memoizedState !== null) !=
              (n !== null && n.memoizedState !== null) &&
            (hc = Kt()),
          a & 4 &&
            ((a = t.updateQueue),
            a !== null && ((t.updateQueue = null), fc(t, a))));
        break;
      case 22:
        c = t.memoizedState !== null;
        var T = n !== null && n.memoizedState !== null,
          X = Un,
          P = ce;
        if (
          ((Un = X || c),
          (ce = P || T),
          Te(e, t),
          (ce = P),
          (Un = X),
          ze(t),
          a & 8192)
        )
          t: for (
            e = t.stateNode,
              e._visibility = c ? e._visibility & -2 : e._visibility | 1,
              c && (n === null || T || Un || ce || Fl(t)),
              n = null,
              e = t;
            ;
          ) {
            if (e.tag === 5 || e.tag === 26) {
              if (n === null) {
                T = n = e;
                try {
                  if (((r = T.stateNode), c))
                    ((h = r.style),
                      typeof h.setProperty == "function"
                        ? h.setProperty("display", "none", "important")
                        : (h.display = "none"));
                  else {
                    v = T.stateNode;
                    var lt = T.memoizedProps.style,
                      V =
                        lt != null && lt.hasOwnProperty("display")
                          ? lt.display
                          : null;
                    v.style.display =
                      V == null || typeof V == "boolean" ? "" : ("" + V).trim();
                  }
                } catch (mt) {
                  Ut(T, T.return, mt);
                }
              }
            } else if (e.tag === 6) {
              if (n === null) {
                T = e;
                try {
                  T.stateNode.nodeValue = c ? "" : T.memoizedProps;
                } catch (mt) {
                  Ut(T, T.return, mt);
                }
              }
            } else if (e.tag === 18) {
              if (n === null) {
                T = e;
                try {
                  var K = T.stateNode;
                  c ? Lm(K, !0) : Lm(T.stateNode, !1);
                } catch (mt) {
                  Ut(T, T.return, mt);
                }
              }
            } else if (
              ((e.tag !== 22 && e.tag !== 23) ||
                e.memoizedState === null ||
                e === t) &&
              e.child !== null
            ) {
              ((e.child.return = e), (e = e.child));
              continue;
            }
            if (e === t) break t;
            for (; e.sibling === null; ) {
              if (e.return === null || e.return === t) break t;
              (n === e && (n = null), (e = e.return));
            }
            (n === e && (n = null),
              (e.sibling.return = e.return),
              (e = e.sibling));
          }
        a & 4 &&
          ((a = t.updateQueue),
          a !== null &&
            ((n = a.retryQueue),
            n !== null && ((a.retryQueue = null), fc(t, n))));
        break;
      case 19:
        (Te(e, t),
          ze(t),
          a & 4 &&
            ((a = t.updateQueue),
            a !== null && ((t.updateQueue = null), fc(t, a))));
        break;
      case 30:
        break;
      case 21:
        break;
      default:
        (Te(e, t), ze(t));
    }
  }
  function ze(t) {
    var e = t.flags;
    if (e & 2) {
      try {
        for (var n, a = t.return; a !== null; ) {
          if (Zh(a)) {
            n = a;
            break;
          }
          a = a.return;
        }
        if (n == null) throw Error(s(160));
        switch (n.tag) {
          case 27:
            var c = n.stateNode,
              r = Xr(t);
            oc(t, r, c);
            break;
          case 5:
            var h = n.stateNode;
            n.flags & 32 && (sa(h, ""), (n.flags &= -33));
            var v = Xr(t);
            oc(t, v, h);
            break;
          case 3:
          case 4:
            var T = n.stateNode.containerInfo,
              X = Xr(t);
            Vr(t, X, T);
            break;
          default:
            throw Error(s(161));
        }
      } catch (P) {
        Ut(t, t.return, P);
      }
      t.flags &= -3;
    }
    e & 4096 && (t.flags &= -4097);
  }
  function Ph(t) {
    if (t.subtreeFlags & 1024)
      for (t = t.child; t !== null; ) {
        var e = t;
        (Ph(e),
          e.tag === 5 && e.flags & 1024 && e.stateNode.reset(),
          (t = t.sibling));
      }
  }
  function Yn(t, e) {
    if (e.subtreeFlags & 8772)
      for (e = e.child; e !== null; ) (Kh(t, e.alternate, e), (e = e.sibling));
  }
  function Fl(t) {
    for (t = t.child; t !== null; ) {
      var e = t;
      switch (e.tag) {
        case 0:
        case 11:
        case 14:
        case 15:
          (hl(4, e, e.return), Fl(e));
          break;
        case 1:
          pn(e, e.return);
          var n = e.stateNode;
          (typeof n.componentWillUnmount == "function" && Lh(e, e.return, n),
            Fl(e));
          break;
        case 27:
          Xu(e.stateNode);
        case 26:
        case 5:
          (pn(e, e.return), Fl(e));
          break;
        case 22:
          e.memoizedState === null && Fl(e);
          break;
        case 30:
          Fl(e);
          break;
        default:
          Fl(e);
      }
      t = t.sibling;
    }
  }
  function qn(t, e, n) {
    for (n = n && (e.subtreeFlags & 8772) !== 0, e = e.child; e !== null; ) {
      var a = e.alternate,
        c = t,
        r = e,
        h = r.flags;
      switch (r.tag) {
        case 0:
        case 11:
        case 15:
          (qn(c, r, n), Cu(4, r));
          break;
        case 1:
          if (
            (qn(c, r, n),
            (a = r),
            (c = a.stateNode),
            typeof c.componentDidMount == "function")
          )
            try {
              c.componentDidMount();
            } catch (X) {
              Ut(a, a.return, X);
            }
          if (((a = r), (c = a.updateQueue), c !== null)) {
            var v = a.stateNode;
            try {
              var T = c.shared.hiddenCallbacks;
              if (T !== null)
                for (c.shared.hiddenCallbacks = null, c = 0; c < T.length; c++)
                  Cd(T[c], v);
            } catch (X) {
              Ut(a, a.return, X);
            }
          }
          (n && h & 64 && Vh(r), Du(r, r.return));
          break;
        case 27:
          Qh(r);
        case 26:
        case 5:
          (qn(c, r, n), n && a === null && h & 4 && Gh(r), Du(r, r.return));
          break;
        case 12:
          qn(c, r, n);
          break;
        case 31:
          (qn(c, r, n), n && h & 4 && Wh(c, r));
          break;
        case 13:
          (qn(c, r, n), n && h & 4 && Fh(c, r));
          break;
        case 22:
          (r.memoizedState === null && qn(c, r, n), Du(r, r.return));
          break;
        case 30:
          break;
        default:
          qn(c, r, n);
      }
      e = e.sibling;
    }
  }
  function Gr(t, e) {
    var n = null;
    (t !== null &&
      t.memoizedState !== null &&
      t.memoizedState.cachePool !== null &&
      (n = t.memoizedState.cachePool.pool),
      (t = null),
      e.memoizedState !== null &&
        e.memoizedState.cachePool !== null &&
        (t = e.memoizedState.cachePool.pool),
      t !== n && (t != null && t.refCount++, n != null && pu(n)));
  }
  function Zr(t, e) {
    ((t = null),
      e.alternate !== null && (t = e.alternate.memoizedState.cache),
      (e = e.memoizedState.cache),
      e !== t && (e.refCount++, t != null && pu(t)));
  }
  function un(t, e, n, a) {
    if (e.subtreeFlags & 10256)
      for (e = e.child; e !== null; ) (tm(t, e, n, a), (e = e.sibling));
  }
  function tm(t, e, n, a) {
    var c = e.flags;
    switch (e.tag) {
      case 0:
      case 11:
      case 15:
        (un(t, e, n, a), c & 2048 && Cu(9, e));
        break;
      case 1:
        un(t, e, n, a);
        break;
      case 3:
        (un(t, e, n, a),
          c & 2048 &&
            ((t = null),
            e.alternate !== null && (t = e.alternate.memoizedState.cache),
            (e = e.memoizedState.cache),
            e !== t && (e.refCount++, t != null && pu(t))));
        break;
      case 12:
        if (c & 2048) {
          (un(t, e, n, a), (t = e.stateNode));
          try {
            var r = e.memoizedProps,
              h = r.id,
              v = r.onPostCommit;
            typeof v == "function" &&
              v(
                h,
                e.alternate === null ? "mount" : "update",
                t.passiveEffectDuration,
                -0,
              );
          } catch (T) {
            Ut(e, e.return, T);
          }
        } else un(t, e, n, a);
        break;
      case 31:
        un(t, e, n, a);
        break;
      case 13:
        un(t, e, n, a);
        break;
      case 23:
        break;
      case 22:
        ((r = e.stateNode),
          (h = e.alternate),
          e.memoizedState !== null
            ? r._visibility & 2
              ? un(t, e, n, a)
              : Ou(t, e)
            : r._visibility & 2
              ? un(t, e, n, a)
              : ((r._visibility |= 2),
                za(t, e, n, a, (e.subtreeFlags & 10256) !== 0 || !1)),
          c & 2048 && Gr(h, e));
        break;
      case 24:
        (un(t, e, n, a), c & 2048 && Zr(e.alternate, e));
        break;
      default:
        un(t, e, n, a);
    }
  }
  function za(t, e, n, a, c) {
    for (
      c = c && ((e.subtreeFlags & 10256) !== 0 || !1), e = e.child;
      e !== null;
    ) {
      var r = t,
        h = e,
        v = n,
        T = a,
        X = h.flags;
      switch (h.tag) {
        case 0:
        case 11:
        case 15:
          (za(r, h, v, T, c), Cu(8, h));
          break;
        case 23:
          break;
        case 22:
          var P = h.stateNode;
          (h.memoizedState !== null
            ? P._visibility & 2
              ? za(r, h, v, T, c)
              : Ou(r, h)
            : ((P._visibility |= 2), za(r, h, v, T, c)),
            c && X & 2048 && Gr(h.alternate, h));
          break;
        case 24:
          (za(r, h, v, T, c), c && X & 2048 && Zr(h.alternate, h));
          break;
        default:
          za(r, h, v, T, c);
      }
      e = e.sibling;
    }
  }
  function Ou(t, e) {
    if (e.subtreeFlags & 10256)
      for (e = e.child; e !== null; ) {
        var n = t,
          a = e,
          c = a.flags;
        switch (a.tag) {
          case 22:
            (Ou(n, a), c & 2048 && Gr(a.alternate, a));
            break;
          case 24:
            (Ou(n, a), c & 2048 && Zr(a.alternate, a));
            break;
          default:
            Ou(n, a);
        }
        e = e.sibling;
      }
  }
  var Ru = 8192;
  function Ma(t, e, n) {
    if (t.subtreeFlags & Ru)
      for (t = t.child; t !== null; ) (em(t, e, n), (t = t.sibling));
  }
  function em(t, e, n) {
    switch (t.tag) {
      case 26:
        (Ma(t, e, n),
          t.flags & Ru &&
            t.memoizedState !== null &&
            Kv(n, an, t.memoizedState, t.memoizedProps));
        break;
      case 5:
        Ma(t, e, n);
        break;
      case 3:
      case 4:
        var a = an;
        ((an = Nc(t.stateNode.containerInfo)), Ma(t, e, n), (an = a));
        break;
      case 22:
        t.memoizedState === null &&
          ((a = t.alternate),
          a !== null && a.memoizedState !== null
            ? ((a = Ru), (Ru = 16777216), Ma(t, e, n), (Ru = a))
            : Ma(t, e, n));
        break;
      default:
        Ma(t, e, n);
    }
  }
  function nm(t) {
    var e = t.alternate;
    if (e !== null && ((t = e.child), t !== null)) {
      e.child = null;
      do ((e = t.sibling), (t.sibling = null), (t = e));
      while (t !== null);
    }
  }
  function Hu(t) {
    var e = t.deletions;
    if ((t.flags & 16) !== 0) {
      if (e !== null)
        for (var n = 0; n < e.length; n++) {
          var a = e[n];
          ((he = a), am(a, t));
        }
      nm(t);
    }
    if (t.subtreeFlags & 10256)
      for (t = t.child; t !== null; ) (lm(t), (t = t.sibling));
  }
  function lm(t) {
    switch (t.tag) {
      case 0:
      case 11:
      case 15:
        (Hu(t), t.flags & 2048 && hl(9, t, t.return));
        break;
      case 3:
        Hu(t);
        break;
      case 12:
        Hu(t);
        break;
      case 22:
        var e = t.stateNode;
        t.memoizedState !== null &&
        e._visibility & 2 &&
        (t.return === null || t.return.tag !== 13)
          ? ((e._visibility &= -3), dc(t))
          : Hu(t);
        break;
      default:
        Hu(t);
    }
  }
  function dc(t) {
    var e = t.deletions;
    if ((t.flags & 16) !== 0) {
      if (e !== null)
        for (var n = 0; n < e.length; n++) {
          var a = e[n];
          ((he = a), am(a, t));
        }
      nm(t);
    }
    for (t = t.child; t !== null; ) {
      switch (((e = t), e.tag)) {
        case 0:
        case 11:
        case 15:
          (hl(8, e, e.return), dc(e));
          break;
        case 22:
          ((n = e.stateNode),
            n._visibility & 2 && ((n._visibility &= -3), dc(e)));
          break;
        default:
          dc(e);
      }
      t = t.sibling;
    }
  }
  function am(t, e) {
    for (; he !== null; ) {
      var n = he;
      switch (n.tag) {
        case 0:
        case 11:
        case 15:
          hl(8, n, e);
          break;
        case 23:
        case 22:
          if (n.memoizedState !== null && n.memoizedState.cachePool !== null) {
            var a = n.memoizedState.cachePool.pool;
            a != null && a.refCount++;
          }
          break;
        case 24:
          pu(n.memoizedState.cache);
      }
      if (((a = n.child), a !== null)) ((a.return = n), (he = a));
      else
        t: for (n = t; he !== null; ) {
          a = he;
          var c = a.sibling,
            r = a.return;
          if (($h(a), a === n)) {
            he = null;
            break t;
          }
          if (c !== null) {
            ((c.return = r), (he = c));
            break t;
          }
          he = r;
        }
    }
  }
  var sv = {
      getCacheForType: function (t) {
        var e = pe(ae),
          n = e.data.get(t);
        return (n === void 0 && ((n = t()), e.data.set(t, n)), n);
      },
      cacheSignal: function () {
        return pe(ae).controller.signal;
      },
    },
    rv = typeof WeakMap == "function" ? WeakMap : Map,
    Ht = 0,
    Xt = null,
    At = null,
    zt = 0,
    jt = 0,
    qe = null,
    ml = !1,
    Ca = !1,
    Qr = !1,
    Xn = 0,
    Wt = 0,
    gl = 0,
    Il = 0,
    kr = 0,
    Xe = 0,
    Da = 0,
    ju = null,
    Me = null,
    Kr = !1,
    hc = 0,
    um = 0,
    mc = 1 / 0,
    gc = null,
    yl = null,
    oe = 0,
    pl = null,
    Oa = null,
    Vn = 0,
    $r = 0,
    Jr = null,
    im = null,
    Uu = 0,
    Wr = null;
  function Ve() {
    return (Ht & 2) !== 0 && zt !== 0 ? zt & -zt : S.T !== null ? no() : xi();
  }
  function cm() {
    if (Xe === 0)
      if ((zt & 536870912) === 0 || Ct) {
        var t = Pn;
        ((Pn <<= 1), (Pn & 3932160) === 0 && (Pn = 262144), (Xe = t));
      } else Xe = 536870912;
    return ((t = Be.current), t !== null && (t.flags |= 32), Xe);
  }
  function Ce(t, e, n) {
    (((t === Xt && (jt === 2 || jt === 9)) || t.cancelPendingCommit !== null) &&
      (Ra(t, 0), vl(t, zt, Xe, !1)),
      Hl(t, n),
      ((Ht & 2) === 0 || t !== Xt) &&
        (t === Xt &&
          ((Ht & 2) === 0 && (Il |= n), Wt === 4 && vl(t, zt, Xe, !1)),
        vn(t)));
  }
  function sm(t, e, n) {
    if ((Ht & 6) !== 0) throw Error(s(327));
    var a = (!n && (e & 127) === 0 && (e & t.expiredLanes) === 0) || Rl(t, e),
      c = a ? dv(t, e) : Ir(t, e, !0),
      r = a;
    do {
      if (c === 0) {
        Ca && !a && vl(t, e, 0, !1);
        break;
      } else {
        if (((n = t.current.alternate), r && !ov(n))) {
          ((c = Ir(t, e, !1)), (r = !1));
          continue;
        }
        if (c === 2) {
          if (((r = e), t.errorRecoveryDisabledLanes & r)) var h = 0;
          else
            ((h = t.pendingLanes & -536870913),
              (h = h !== 0 ? h : h & 536870912 ? 536870912 : 0));
          if (h !== 0) {
            e = h;
            t: {
              var v = t;
              c = ju;
              var T = v.current.memoizedState.isDehydrated;
              if ((T && (Ra(v, h).flags |= 256), (h = Ir(v, h, !1)), h !== 2)) {
                if (Qr && !T) {
                  ((v.errorRecoveryDisabledLanes |= r), (Il |= r), (c = 4));
                  break t;
                }
                ((r = Me),
                  (Me = c),
                  r !== null &&
                    (Me === null ? (Me = r) : Me.push.apply(Me, r)));
              }
              c = h;
            }
            if (((r = !1), c !== 2)) continue;
          }
        }
        if (c === 1) {
          (Ra(t, 0), vl(t, e, 0, !0));
          break;
        }
        t: {
          switch (((a = t), (r = c), r)) {
            case 0:
            case 1:
              throw Error(s(345));
            case 4:
              if ((e & 4194048) !== e) break;
            case 6:
              vl(a, e, Xe, !ml);
              break t;
            case 2:
              Me = null;
              break;
            case 3:
            case 5:
              break;
            default:
              throw Error(s(329));
          }
          if ((e & 62914560) === e && ((c = hc + 300 - Kt()), 10 < c)) {
            if ((vl(a, e, Xe, !ml), ia(a, 0, !0) !== 0)) break t;
            ((Vn = e),
              (a.timeoutHandle = qm(
                rm.bind(
                  null,
                  a,
                  n,
                  Me,
                  gc,
                  Kr,
                  e,
                  Xe,
                  Il,
                  Da,
                  ml,
                  r,
                  "Throttled",
                  -0,
                  0,
                ),
                c,
              )));
            break t;
          }
          rm(a, n, Me, gc, Kr, e, Xe, Il, Da, ml, r, null, -0, 0);
        }
      }
      break;
    } while (!0);
    vn(t);
  }
  function rm(t, e, n, a, c, r, h, v, T, X, P, lt, V, K) {
    if (
      ((t.timeoutHandle = -1),
      (lt = e.subtreeFlags),
      lt & 8192 || (lt & 16785408) === 16785408)
    ) {
      ((lt = {
        stylesheets: null,
        count: 0,
        imgCount: 0,
        imgBytes: 0,
        suspenseyImages: [],
        waitingForImages: !0,
        waitingForViewTransition: !1,
        unsuspend: An,
      }),
        em(e, r, lt));
      var mt =
        (r & 62914560) === r ? hc - Kt() : (r & 4194048) === r ? um - Kt() : 0;
      if (((mt = $v(lt, mt)), mt !== null)) {
        ((Vn = r),
          (t.cancelPendingCommit = mt(
            pm.bind(null, t, e, r, n, a, c, h, v, T, P, lt, null, V, K),
          )),
          vl(t, r, h, !X));
        return;
      }
    }
    pm(t, e, r, n, a, c, h, v, T);
  }
  function ov(t) {
    for (var e = t; ; ) {
      var n = e.tag;
      if (
        (n === 0 || n === 11 || n === 15) &&
        e.flags & 16384 &&
        ((n = e.updateQueue), n !== null && ((n = n.stores), n !== null))
      )
        for (var a = 0; a < n.length; a++) {
          var c = n[a],
            r = c.getSnapshot;
          c = c.value;
          try {
            if (!je(r(), c)) return !1;
          } catch {
            return !1;
          }
        }
      if (((n = e.child), e.subtreeFlags & 16384 && n !== null))
        ((n.return = e), (e = n));
      else {
        if (e === t) break;
        for (; e.sibling === null; ) {
          if (e.return === null || e.return === t) return !0;
          e = e.return;
        }
        ((e.sibling.return = e.return), (e = e.sibling));
      }
    }
    return !0;
  }
  function vl(t, e, n, a) {
    ((e &= ~kr),
      (e &= ~Il),
      (t.suspendedLanes |= e),
      (t.pingedLanes &= ~e),
      a && (t.warmLanes |= e),
      (a = t.expirationTimes));
    for (var c = e; 0 < c; ) {
      var r = 31 - $t(c),
        h = 1 << r;
      ((a[r] = -1), (c &= ~h));
    }
    n !== 0 && yi(t, n, e);
  }
  function yc() {
    return (Ht & 6) === 0 ? (Bu(0), !1) : !0;
  }
  function Fr() {
    if (At !== null) {
      if (jt === 0) var t = At.return;
      else ((t = At), (Cn = Gl = null), hr(t), (_a = null), (xu = 0), (t = At));
      for (; t !== null; ) (Xh(t.alternate, t), (t = t.return));
      At = null;
    }
  }
  function Ra(t, e) {
    var n = t.timeoutHandle;
    (n !== -1 && ((t.timeoutHandle = -1), Cv(n)),
      (n = t.cancelPendingCommit),
      n !== null && ((t.cancelPendingCommit = null), n()),
      (Vn = 0),
      Fr(),
      (Xt = t),
      (At = n = zn(t.current, null)),
      (zt = e),
      (jt = 0),
      (qe = null),
      (ml = !1),
      (Ca = Rl(t, e)),
      (Qr = !1),
      (Da = Xe = kr = Il = gl = Wt = 0),
      (Me = ju = null),
      (Kr = !1),
      (e & 8) !== 0 && (e |= e & 32));
    var a = t.entangledLanes;
    if (a !== 0)
      for (t = t.entanglements, a &= e; 0 < a; ) {
        var c = 31 - $t(a),
          r = 1 << c;
        ((e |= t[c]), (a &= ~r));
      }
    return ((Xn = e), Bi(), n);
  }
  function om(t, e) {
    ((Et = null),
      (S.H = Tu),
      e === Ea || e === Qi
        ? ((e = Ad()), (jt = 3))
        : e === er
          ? ((e = Ad()), (jt = 4))
          : (jt =
              e === Mr
                ? 8
                : e !== null &&
                    typeof e == "object" &&
                    typeof e.then == "function"
                  ? 6
                  : 1),
      (qe = e),
      At === null && ((Wt = 1), uc(t, Ke(e, t.current))));
  }
  function fm() {
    var t = Be.current;
    return t === null
      ? !0
      : (zt & 4194048) === zt
        ? Fe === null
        : (zt & 62914560) === zt || (zt & 536870912) !== 0
          ? t === Fe
          : !1;
  }
  function dm() {
    var t = S.H;
    return ((S.H = Tu), t === null ? Tu : t);
  }
  function hm() {
    var t = S.A;
    return ((S.A = sv), t);
  }
  function pc() {
    ((Wt = 4),
      ml || ((zt & 4194048) !== zt && Be.current !== null) || (Ca = !0),
      ((gl & 134217727) === 0 && (Il & 134217727) === 0) ||
        Xt === null ||
        vl(Xt, zt, Xe, !1));
  }
  function Ir(t, e, n) {
    var a = Ht;
    Ht |= 2;
    var c = dm(),
      r = hm();
    ((Xt !== t || zt !== e) && ((gc = null), Ra(t, e)), (e = !1));
    var h = Wt;
    t: do
      try {
        if (jt !== 0 && At !== null) {
          var v = At,
            T = qe;
          switch (jt) {
            case 8:
              (Fr(), (h = 6));
              break t;
            case 3:
            case 2:
            case 9:
            case 6:
              Be.current === null && (e = !0);
              var X = jt;
              if (((jt = 0), (qe = null), Ha(t, v, T, X), n && Ca)) {
                h = 0;
                break t;
              }
              break;
            default:
              ((X = jt), (jt = 0), (qe = null), Ha(t, v, T, X));
          }
        }
        (fv(), (h = Wt));
        break;
      } catch (P) {
        om(t, P);
      }
    while (!0);
    return (
      e && t.shellSuspendCounter++,
      (Cn = Gl = null),
      (Ht = a),
      (S.H = c),
      (S.A = r),
      At === null && ((Xt = null), (zt = 0), Bi()),
      h
    );
  }
  function fv() {
    for (; At !== null; ) mm(At);
  }
  function dv(t, e) {
    var n = Ht;
    Ht |= 2;
    var a = dm(),
      c = hm();
    Xt !== t || zt !== e
      ? ((gc = null), (mc = Kt() + 500), Ra(t, e))
      : (Ca = Rl(t, e));
    t: do
      try {
        if (jt !== 0 && At !== null) {
          e = At;
          var r = qe;
          e: switch (jt) {
            case 1:
              ((jt = 0), (qe = null), Ha(t, e, r, 1));
              break;
            case 2:
            case 9:
              if (wd(r)) {
                ((jt = 0), (qe = null), gm(e));
                break;
              }
              ((e = function () {
                ((jt !== 2 && jt !== 9) || Xt !== t || (jt = 7), vn(t));
              }),
                r.then(e, e));
              break t;
            case 3:
              jt = 7;
              break t;
            case 4:
              jt = 5;
              break t;
            case 7:
              wd(r)
                ? ((jt = 0), (qe = null), gm(e))
                : ((jt = 0), (qe = null), Ha(t, e, r, 7));
              break;
            case 5:
              var h = null;
              switch (At.tag) {
                case 26:
                  h = At.memoizedState;
                case 5:
                case 27:
                  var v = At;
                  if (h ? t0(h) : v.stateNode.complete) {
                    ((jt = 0), (qe = null));
                    var T = v.sibling;
                    if (T !== null) At = T;
                    else {
                      var X = v.return;
                      X !== null ? ((At = X), vc(X)) : (At = null);
                    }
                    break e;
                  }
              }
              ((jt = 0), (qe = null), Ha(t, e, r, 5));
              break;
            case 6:
              ((jt = 0), (qe = null), Ha(t, e, r, 6));
              break;
            case 8:
              (Fr(), (Wt = 6));
              break t;
            default:
              throw Error(s(462));
          }
        }
        hv();
        break;
      } catch (P) {
        om(t, P);
      }
    while (!0);
    return (
      (Cn = Gl = null),
      (S.H = a),
      (S.A = c),
      (Ht = n),
      At !== null ? 0 : ((Xt = null), (zt = 0), Bi(), Wt)
    );
  }
  function hv() {
    for (; At !== null && !$n(); ) mm(At);
  }
  function mm(t) {
    var e = Yh(t.alternate, t, Xn);
    ((t.memoizedProps = t.pendingProps), e === null ? vc(t) : (At = e));
  }
  function gm(t) {
    var e = t,
      n = e.alternate;
    switch (e.tag) {
      case 15:
      case 0:
        e = Oh(n, e, e.pendingProps, e.type, void 0, zt);
        break;
      case 11:
        e = Oh(n, e, e.pendingProps, e.type.render, e.ref, zt);
        break;
      case 5:
        hr(e);
      default:
        (Xh(n, e), (e = At = hd(e, Xn)), (e = Yh(n, e, Xn)));
    }
    ((t.memoizedProps = t.pendingProps), e === null ? vc(t) : (At = e));
  }
  function Ha(t, e, n, a) {
    ((Cn = Gl = null), hr(e), (_a = null), (xu = 0));
    var c = e.return;
    try {
      if (ev(t, c, e, n, zt)) {
        ((Wt = 1), uc(t, Ke(n, t.current)), (At = null));
        return;
      }
    } catch (r) {
      if (c !== null) throw ((At = c), r);
      ((Wt = 1), uc(t, Ke(n, t.current)), (At = null));
      return;
    }
    e.flags & 32768
      ? (Ct || a === 1
          ? (t = !0)
          : Ca || (zt & 536870912) !== 0
            ? (t = !1)
            : ((ml = t = !0),
              (a === 2 || a === 9 || a === 3 || a === 6) &&
                ((a = Be.current),
                a !== null && a.tag === 13 && (a.flags |= 16384))),
        ym(e, t))
      : vc(e);
  }
  function vc(t) {
    var e = t;
    do {
      if ((e.flags & 32768) !== 0) {
        ym(e, ml);
        return;
      }
      t = e.return;
      var n = av(e.alternate, e, Xn);
      if (n !== null) {
        At = n;
        return;
      }
      if (((e = e.sibling), e !== null)) {
        At = e;
        return;
      }
      At = e = t;
    } while (e !== null);
    Wt === 0 && (Wt = 5);
  }
  function ym(t, e) {
    do {
      var n = uv(t.alternate, t);
      if (n !== null) {
        ((n.flags &= 32767), (At = n));
        return;
      }
      if (
        ((n = t.return),
        n !== null &&
          ((n.flags |= 32768), (n.subtreeFlags = 0), (n.deletions = null)),
        !e && ((t = t.sibling), t !== null))
      ) {
        At = t;
        return;
      }
      At = t = n;
    } while (t !== null);
    ((Wt = 6), (At = null));
  }
  function pm(t, e, n, a, c, r, h, v, T) {
    t.cancelPendingCommit = null;
    do xc();
    while (oe !== 0);
    if ((Ht & 6) !== 0) throw Error(s(327));
    if (e !== null) {
      if (e === t.current) throw Error(s(177));
      if (
        ((r = e.lanes | e.childLanes),
        (r |= Xs),
        gs(t, n, r, h, v, T),
        t === Xt && ((At = Xt = null), (zt = 0)),
        (Oa = e),
        (pl = t),
        (Vn = n),
        ($r = r),
        (Jr = c),
        (im = a),
        (e.subtreeFlags & 10256) !== 0 || (e.flags & 10256) !== 0
          ? ((t.callbackNode = null),
            (t.callbackPriority = 0),
            pv(nn, function () {
              return (Em(), null);
            }))
          : ((t.callbackNode = null), (t.callbackPriority = 0)),
        (a = (e.flags & 13878) !== 0),
        (e.subtreeFlags & 13878) !== 0 || a)
      ) {
        ((a = S.T), (S.T = null), (c = R.p), (R.p = 2), (h = Ht), (Ht |= 4));
        try {
          iv(t, e, n);
        } finally {
          ((Ht = h), (R.p = c), (S.T = a));
        }
      }
      ((oe = 1), vm(), xm(), bm());
    }
  }
  function vm() {
    if (oe === 1) {
      oe = 0;
      var t = pl,
        e = Oa,
        n = (e.flags & 13878) !== 0;
      if ((e.subtreeFlags & 13878) !== 0 || n) {
        ((n = S.T), (S.T = null));
        var a = R.p;
        R.p = 2;
        var c = Ht;
        Ht |= 4;
        try {
          Ih(e, t);
          var r = oo,
            h = ad(t.containerInfo),
            v = r.focusedElem,
            T = r.selectionRange;
          if (
            h !== v &&
            v &&
            v.ownerDocument &&
            ld(v.ownerDocument.documentElement, v)
          ) {
            if (T !== null && js(v)) {
              var X = T.start,
                P = T.end;
              if ((P === void 0 && (P = X), "selectionStart" in v))
                ((v.selectionStart = X),
                  (v.selectionEnd = Math.min(P, v.value.length)));
              else {
                var lt = v.ownerDocument || document,
                  V = (lt && lt.defaultView) || window;
                if (V.getSelection) {
                  var K = V.getSelection(),
                    mt = v.textContent.length,
                    xt = Math.min(T.start, mt),
                    qt = T.end === void 0 ? xt : Math.min(T.end, mt);
                  !K.extend && xt > qt && ((h = qt), (qt = xt), (xt = h));
                  var U = nd(v, xt),
                    C = nd(v, qt);
                  if (
                    U &&
                    C &&
                    (K.rangeCount !== 1 ||
                      K.anchorNode !== U.node ||
                      K.anchorOffset !== U.offset ||
                      K.focusNode !== C.node ||
                      K.focusOffset !== C.offset)
                  ) {
                    var q = lt.createRange();
                    (q.setStart(U.node, U.offset),
                      K.removeAllRanges(),
                      xt > qt
                        ? (K.addRange(q), K.extend(C.node, C.offset))
                        : (q.setEnd(C.node, C.offset), K.addRange(q)));
                  }
                }
              }
            }
            for (lt = [], K = v; (K = K.parentNode); )
              K.nodeType === 1 &&
                lt.push({ element: K, left: K.scrollLeft, top: K.scrollTop });
            for (
              typeof v.focus == "function" && v.focus(), v = 0;
              v < lt.length;
              v++
            ) {
              var nt = lt[v];
              ((nt.element.scrollLeft = nt.left),
                (nt.element.scrollTop = nt.top));
            }
          }
          ((Dc = !!ro), (oo = ro = null));
        } finally {
          ((Ht = c), (R.p = a), (S.T = n));
        }
      }
      ((t.current = e), (oe = 2));
    }
  }
  function xm() {
    if (oe === 2) {
      oe = 0;
      var t = pl,
        e = Oa,
        n = (e.flags & 8772) !== 0;
      if ((e.subtreeFlags & 8772) !== 0 || n) {
        ((n = S.T), (S.T = null));
        var a = R.p;
        R.p = 2;
        var c = Ht;
        Ht |= 4;
        try {
          Kh(t, e.alternate, e);
        } finally {
          ((Ht = c), (R.p = a), (S.T = n));
        }
      }
      oe = 3;
    }
  }
  function bm() {
    if (oe === 4 || oe === 3) {
      ((oe = 0), Jn());
      var t = pl,
        e = Oa,
        n = Vn,
        a = im;
      (e.subtreeFlags & 10256) !== 0 || (e.flags & 10256) !== 0
        ? (oe = 5)
        : ((oe = 0), (Oa = pl = null), Sm(t, t.pendingLanes));
      var c = t.pendingLanes;
      if (
        (c === 0 && (yl = null),
        nu(n),
        (e = e.stateNode),
        Pt && typeof Pt.onCommitFiberRoot == "function")
      )
        try {
          Pt.onCommitFiberRoot(He, e, void 0, (e.current.flags & 128) === 128);
        } catch {}
      if (a !== null) {
        ((e = S.T), (c = R.p), (R.p = 2), (S.T = null));
        try {
          for (var r = t.onRecoverableError, h = 0; h < a.length; h++) {
            var v = a[h];
            r(v.value, { componentStack: v.stack });
          }
        } finally {
          ((S.T = e), (R.p = c));
        }
      }
      ((Vn & 3) !== 0 && xc(),
        vn(t),
        (c = t.pendingLanes),
        (n & 261930) !== 0 && (c & 42) !== 0
          ? t === Wr
            ? Uu++
            : ((Uu = 0), (Wr = t))
          : (Uu = 0),
        Bu(0));
    }
  }
  function Sm(t, e) {
    (t.pooledCacheLanes &= e) === 0 &&
      ((e = t.pooledCache), e != null && ((t.pooledCache = null), pu(e)));
  }
  function xc() {
    return (vm(), xm(), bm(), Em());
  }
  function Em() {
    if (oe !== 5) return !1;
    var t = pl,
      e = $r;
    $r = 0;
    var n = nu(Vn),
      a = S.T,
      c = R.p;
    try {
      ((R.p = 32 > n ? 32 : n), (S.T = null), (n = Jr), (Jr = null));
      var r = pl,
        h = Vn;
      if (((oe = 0), (Oa = pl = null), (Vn = 0), (Ht & 6) !== 0))
        throw Error(s(331));
      var v = Ht;
      if (
        ((Ht |= 4),
        lm(r.current),
        tm(r, r.current, h, n),
        (Ht = v),
        Bu(0, !1),
        Pt && typeof Pt.onPostCommitFiberRoot == "function")
      )
        try {
          Pt.onPostCommitFiberRoot(He, r);
        } catch {}
      return !0;
    } finally {
      ((R.p = c), (S.T = a), Sm(t, e));
    }
  }
  function _m(t, e, n) {
    ((e = Ke(n, e)),
      (e = zr(t.stateNode, e, 2)),
      (t = ol(t, e, 2)),
      t !== null && (Hl(t, 2), vn(t)));
  }
  function Ut(t, e, n) {
    if (t.tag === 3) _m(t, t, n);
    else
      for (; e !== null; ) {
        if (e.tag === 3) {
          _m(e, t, n);
          break;
        } else if (e.tag === 1) {
          var a = e.stateNode;
          if (
            typeof e.type.getDerivedStateFromError == "function" ||
            (typeof a.componentDidCatch == "function" &&
              (yl === null || !yl.has(a)))
          ) {
            ((t = Ke(n, t)),
              (n = wh(2)),
              (a = ol(e, n, 2)),
              a !== null && (Nh(n, a, e, t), Hl(a, 2), vn(a)));
            break;
          }
        }
        e = e.return;
      }
  }
  function Pr(t, e, n) {
    var a = t.pingCache;
    if (a === null) {
      a = t.pingCache = new rv();
      var c = new Set();
      a.set(e, c);
    } else ((c = a.get(e)), c === void 0 && ((c = new Set()), a.set(e, c)));
    c.has(n) ||
      ((Qr = !0), c.add(n), (t = mv.bind(null, t, e, n)), e.then(t, t));
  }
  function mv(t, e, n) {
    var a = t.pingCache;
    (a !== null && a.delete(e),
      (t.pingedLanes |= t.suspendedLanes & n),
      (t.warmLanes &= ~n),
      Xt === t &&
        (zt & n) === n &&
        (Wt === 4 || (Wt === 3 && (zt & 62914560) === zt && 300 > Kt() - hc)
          ? (Ht & 2) === 0 && Ra(t, 0)
          : (kr |= n),
        Da === zt && (Da = 0)),
      vn(t));
  }
  function wm(t, e) {
    (e === 0 && (e = gi()), (t = Xl(t, e)), t !== null && (Hl(t, e), vn(t)));
  }
  function gv(t) {
    var e = t.memoizedState,
      n = 0;
    (e !== null && (n = e.retryLane), wm(t, n));
  }
  function yv(t, e) {
    var n = 0;
    switch (t.tag) {
      case 31:
      case 13:
        var a = t.stateNode,
          c = t.memoizedState;
        c !== null && (n = c.retryLane);
        break;
      case 19:
        a = t.stateNode;
        break;
      case 22:
        a = t.stateNode._retryCache;
        break;
      default:
        throw Error(s(314));
    }
    (a !== null && a.delete(e), wm(t, n));
  }
  function pv(t, e) {
    return Oe(t, e);
  }
  var bc = null,
    ja = null,
    to = !1,
    Sc = !1,
    eo = !1,
    xl = 0;
  function vn(t) {
    (t !== ja &&
      t.next === null &&
      (ja === null ? (bc = ja = t) : (ja = ja.next = t)),
      (Sc = !0),
      to || ((to = !0), xv()));
  }
  function Bu(t, e) {
    if (!eo && Sc) {
      eo = !0;
      do
        for (var n = !1, a = bc; a !== null; ) {
          if (t !== 0) {
            var c = a.pendingLanes;
            if (c === 0) var r = 0;
            else {
              var h = a.suspendedLanes,
                v = a.pingedLanes;
              ((r = (1 << (31 - $t(42 | t) + 1)) - 1),
                (r &= c & ~(h & ~v)),
                (r = r & 201326741 ? (r & 201326741) | 1 : r ? r | 2 : 0));
            }
            r !== 0 && ((n = !0), zm(a, r));
          } else
            ((r = zt),
              (r = ia(
                a,
                a === Xt ? r : 0,
                a.cancelPendingCommit !== null || a.timeoutHandle !== -1,
              )),
              (r & 3) === 0 || Rl(a, r) || ((n = !0), zm(a, r)));
          a = a.next;
        }
      while (n);
      eo = !1;
    }
  }
  function vv() {
    Nm();
  }
  function Nm() {
    Sc = to = !1;
    var t = 0;
    xl !== 0 && Mv() && (t = xl);
    for (var e = Kt(), n = null, a = bc; a !== null; ) {
      var c = a.next,
        r = Am(a, e);
      (r === 0
        ? ((a.next = null),
          n === null ? (bc = c) : (n.next = c),
          c === null && (ja = n))
        : ((n = a), (t !== 0 || (r & 3) !== 0) && (Sc = !0)),
        (a = c));
    }
    ((oe !== 0 && oe !== 5) || Bu(t), xl !== 0 && (xl = 0));
  }
  function Am(t, e) {
    for (
      var n = t.suspendedLanes,
        a = t.pingedLanes,
        c = t.expirationTimes,
        r = t.pendingLanes & -62914561;
      0 < r;
    ) {
      var h = 31 - $t(r),
        v = 1 << h,
        T = c[h];
      (T === -1
        ? ((v & n) === 0 || (v & a) !== 0) && (c[h] = ms(v, e))
        : T <= e && (t.expiredLanes |= v),
        (r &= ~v));
    }
    if (
      ((e = Xt),
      (n = zt),
      (n = ia(
        t,
        t === e ? n : 0,
        t.cancelPendingCommit !== null || t.timeoutHandle !== -1,
      )),
      (a = t.callbackNode),
      n === 0 ||
        (t === e && (jt === 2 || jt === 9)) ||
        t.cancelPendingCommit !== null)
    )
      return (
        a !== null && a !== null && Sn(a),
        (t.callbackNode = null),
        (t.callbackPriority = 0)
      );
    if ((n & 3) === 0 || Rl(t, n)) {
      if (((e = n & -n), e === t.callbackPriority)) return e;
      switch ((a !== null && Sn(a), nu(n))) {
        case 2:
        case 8:
          n = dn;
          break;
        case 32:
          n = nn;
          break;
        case 268435456:
          n = In;
          break;
        default:
          n = nn;
      }
      return (
        (a = Tm.bind(null, t)),
        (n = Oe(n, a)),
        (t.callbackPriority = e),
        (t.callbackNode = n),
        e
      );
    }
    return (
      a !== null && a !== null && Sn(a),
      (t.callbackPriority = 2),
      (t.callbackNode = null),
      2
    );
  }
  function Tm(t, e) {
    if (oe !== 0 && oe !== 5)
      return ((t.callbackNode = null), (t.callbackPriority = 0), null);
    var n = t.callbackNode;
    if (xc() && t.callbackNode !== n) return null;
    var a = zt;
    return (
      (a = ia(
        t,
        t === Xt ? a : 0,
        t.cancelPendingCommit !== null || t.timeoutHandle !== -1,
      )),
      a === 0
        ? null
        : (sm(t, a, e),
          Am(t, Kt()),
          t.callbackNode != null && t.callbackNode === n
            ? Tm.bind(null, t)
            : null)
    );
  }
  function zm(t, e) {
    if (xc()) return null;
    sm(t, e, !0);
  }
  function xv() {
    Dv(function () {
      (Ht & 6) !== 0 ? Oe(fn, vv) : Nm();
    });
  }
  function no() {
    if (xl === 0) {
      var t = ba;
      (t === 0 && ((t = hn), (hn <<= 1), (hn & 261888) === 0 && (hn = 256)),
        (xl = t));
    }
    return xl;
  }
  function Mm(t) {
    return t == null || typeof t == "symbol" || typeof t == "boolean"
      ? null
      : typeof t == "function"
        ? t
        : Mi("" + t);
  }
  function Cm(t, e) {
    var n = e.ownerDocument.createElement("input");
    return (
      (n.name = e.name),
      (n.value = e.value),
      t.id && n.setAttribute("form", t.id),
      e.parentNode.insertBefore(n, e),
      (t = new FormData(t)),
      n.parentNode.removeChild(n),
      t
    );
  }
  function bv(t, e, n, a, c) {
    if (e === "submit" && n && n.stateNode === c) {
      var r = Mm((c[ge] || null).action),
        h = a.submitter;
      h &&
        ((e = (e = h[ge] || null)
          ? Mm(e.formAction)
          : h.getAttribute("formAction")),
        e !== null && ((r = e), (h = null)));
      var v = new Ri("action", "action", null, a, c);
      t.push({
        event: v,
        listeners: [
          {
            instance: null,
            listener: function () {
              if (a.defaultPrevented) {
                if (xl !== 0) {
                  var T = h ? Cm(c, h) : new FormData(c);
                  Er(
                    n,
                    { pending: !0, data: T, method: c.method, action: r },
                    null,
                    T,
                  );
                }
              } else
                typeof r == "function" &&
                  (v.preventDefault(),
                  (T = h ? Cm(c, h) : new FormData(c)),
                  Er(
                    n,
                    { pending: !0, data: T, method: c.method, action: r },
                    r,
                    T,
                  ));
            },
            currentTarget: c,
          },
        ],
      });
    }
  }
  for (var lo = 0; lo < qs.length; lo++) {
    var ao = qs[lo],
      Sv = ao.toLowerCase(),
      Ev = ao[0].toUpperCase() + ao.slice(1);
    ln(Sv, "on" + Ev);
  }
  (ln(cd, "onAnimationEnd"),
    ln(sd, "onAnimationIteration"),
    ln(rd, "onAnimationStart"),
    ln("dblclick", "onDoubleClick"),
    ln("focusin", "onFocus"),
    ln("focusout", "onBlur"),
    ln(Yp, "onTransitionRun"),
    ln(qp, "onTransitionStart"),
    ln(Xp, "onTransitionCancel"),
    ln(od, "onTransitionEnd"),
    wn("onMouseEnter", ["mouseout", "mouseover"]),
    wn("onMouseLeave", ["mouseout", "mouseover"]),
    wn("onPointerEnter", ["pointerout", "pointerover"]),
    wn("onPointerLeave", ["pointerout", "pointerover"]),
    _n(
      "onChange",
      "change click focusin focusout input keydown keyup selectionchange".split(
        " ",
      ),
    ),
    _n(
      "onSelect",
      "focusout contextmenu dragend focusin keydown keyup mousedown mouseup selectionchange".split(
        " ",
      ),
    ),
    _n("onBeforeInput", ["compositionend", "keypress", "textInput", "paste"]),
    _n(
      "onCompositionEnd",
      "compositionend focusout keydown keypress keyup mousedown".split(" "),
    ),
    _n(
      "onCompositionStart",
      "compositionstart focusout keydown keypress keyup mousedown".split(" "),
    ),
    _n(
      "onCompositionUpdate",
      "compositionupdate focusout keydown keypress keyup mousedown".split(" "),
    ));
  var Yu =
      "abort canplay canplaythrough durationchange emptied encrypted ended error loadeddata loadedmetadata loadstart pause play playing progress ratechange resize seeked seeking stalled suspend timeupdate volumechange waiting".split(
        " ",
      ),
    _v = new Set(
      "beforetoggle cancel close invalid load scroll scrollend toggle"
        .split(" ")
        .concat(Yu),
    );
  function Dm(t, e) {
    e = (e & 4) !== 0;
    for (var n = 0; n < t.length; n++) {
      var a = t[n],
        c = a.event;
      a = a.listeners;
      t: {
        var r = void 0;
        if (e)
          for (var h = a.length - 1; 0 <= h; h--) {
            var v = a[h],
              T = v.instance,
              X = v.currentTarget;
            if (((v = v.listener), T !== r && c.isPropagationStopped()))
              break t;
            ((r = v), (c.currentTarget = X));
            try {
              r(c);
            } catch (P) {
              Ui(P);
            }
            ((c.currentTarget = null), (r = T));
          }
        else
          for (h = 0; h < a.length; h++) {
            if (
              ((v = a[h]),
              (T = v.instance),
              (X = v.currentTarget),
              (v = v.listener),
              T !== r && c.isPropagationStopped())
            )
              break t;
            ((r = v), (c.currentTarget = X));
            try {
              r(c);
            } catch (P) {
              Ui(P);
            }
            ((c.currentTarget = null), (r = T));
          }
      }
    }
  }
  function Tt(t, e) {
    var n = e[lu];
    n === void 0 && (n = e[lu] = new Set());
    var a = t + "__bubble";
    n.has(a) || (Om(e, t, 2, !1), n.add(a));
  }
  function uo(t, e, n) {
    var a = 0;
    (e && (a |= 4), Om(n, t, a, e));
  }
  var Ec = "_reactListening" + Math.random().toString(36).slice(2);
  function io(t) {
    if (!t[Ec]) {
      ((t[Ec] = !0),
        Ei.forEach(function (n) {
          n !== "selectionchange" && (_v.has(n) || uo(n, !1, t), uo(n, !0, t));
        }));
      var e = t.nodeType === 9 ? t : t.ownerDocument;
      e === null || e[Ec] || ((e[Ec] = !0), uo("selectionchange", !1, e));
    }
  }
  function Om(t, e, n, a) {
    switch (c0(e)) {
      case 2:
        var c = Fv;
        break;
      case 8:
        c = Iv;
        break;
      default:
        c = Eo;
    }
    ((n = c.bind(null, e, n, t)),
      (c = void 0),
      !As ||
        (e !== "touchstart" && e !== "touchmove" && e !== "wheel") ||
        (c = !0),
      a
        ? c !== void 0
          ? t.addEventListener(e, n, { capture: !0, passive: c })
          : t.addEventListener(e, n, !0)
        : c !== void 0
          ? t.addEventListener(e, n, { passive: c })
          : t.addEventListener(e, n, !1));
  }
  function co(t, e, n, a, c) {
    var r = a;
    if ((e & 1) === 0 && (e & 2) === 0 && a !== null)
      t: for (;;) {
        if (a === null) return;
        var h = a.tag;
        if (h === 3 || h === 4) {
          var v = a.stateNode.containerInfo;
          if (v === c) break;
          if (h === 4)
            for (h = a.return; h !== null; ) {
              var T = h.tag;
              if ((T === 3 || T === 4) && h.stateNode.containerInfo === c)
                return;
              h = h.return;
            }
          for (; v !== null; ) {
            if (((h = el(v)), h === null)) return;
            if (((T = h.tag), T === 5 || T === 6 || T === 26 || T === 27)) {
              a = r = h;
              continue t;
            }
            v = v.parentNode;
          }
        }
        a = a.return;
      }
    Bf(function () {
      var X = r,
        P = ws(n),
        lt = [];
      t: {
        var V = fd.get(t);
        if (V !== void 0) {
          var K = Ri,
            mt = t;
          switch (t) {
            case "keypress":
              if (Di(n) === 0) break t;
            case "keydown":
            case "keyup":
              K = yp;
              break;
            case "focusin":
              ((mt = "focus"), (K = Cs));
              break;
            case "focusout":
              ((mt = "blur"), (K = Cs));
              break;
            case "beforeblur":
            case "afterblur":
              K = Cs;
              break;
            case "click":
              if (n.button === 2) break t;
            case "auxclick":
            case "dblclick":
            case "mousedown":
            case "mousemove":
            case "mouseup":
            case "mouseout":
            case "mouseover":
            case "contextmenu":
              K = Xf;
              break;
            case "drag":
            case "dragend":
            case "dragenter":
            case "dragexit":
            case "dragleave":
            case "dragover":
            case "dragstart":
            case "drop":
              K = ap;
              break;
            case "touchcancel":
            case "touchend":
            case "touchmove":
            case "touchstart":
              K = xp;
              break;
            case cd:
            case sd:
            case rd:
              K = cp;
              break;
            case od:
              K = Sp;
              break;
            case "scroll":
            case "scrollend":
              K = np;
              break;
            case "wheel":
              K = _p;
              break;
            case "copy":
            case "cut":
            case "paste":
              K = rp;
              break;
            case "gotpointercapture":
            case "lostpointercapture":
            case "pointercancel":
            case "pointerdown":
            case "pointermove":
            case "pointerout":
            case "pointerover":
            case "pointerup":
              K = Lf;
              break;
            case "toggle":
            case "beforetoggle":
              K = Np;
          }
          var xt = (e & 4) !== 0,
            qt = !xt && (t === "scroll" || t === "scrollend"),
            U = xt ? (V !== null ? V + "Capture" : null) : V;
          xt = [];
          for (var C = X, q; C !== null; ) {
            var nt = C;
            if (
              ((q = nt.stateNode),
              (nt = nt.tag),
              (nt !== 5 && nt !== 26 && nt !== 27) ||
                q === null ||
                U === null ||
                ((nt = iu(C, U)), nt != null && xt.push(qu(C, nt, q))),
              qt)
            )
              break;
            C = C.return;
          }
          0 < xt.length &&
            ((V = new K(V, mt, null, n, P)),
            lt.push({ event: V, listeners: xt }));
        }
      }
      if ((e & 7) === 0) {
        t: {
          if (
            ((V = t === "mouseover" || t === "pointerover"),
            (K = t === "mouseout" || t === "pointerout"),
            V &&
              n !== _s &&
              (mt = n.relatedTarget || n.fromElement) &&
              (el(mt) || mt[tl]))
          )
            break t;
          if (
            (K || V) &&
            ((V =
              P.window === P
                ? P
                : (V = P.ownerDocument)
                  ? V.defaultView || V.parentWindow
                  : window),
            K
              ? ((mt = n.relatedTarget || n.toElement),
                (K = X),
                (mt = mt ? el(mt) : null),
                mt !== null &&
                  ((qt = f(mt)),
                  (xt = mt.tag),
                  mt !== qt || (xt !== 5 && xt !== 27 && xt !== 6)) &&
                  (mt = null))
              : ((K = null), (mt = X)),
            K !== mt)
          ) {
            if (
              ((xt = Xf),
              (nt = "onMouseLeave"),
              (U = "onMouseEnter"),
              (C = "mouse"),
              (t === "pointerout" || t === "pointerover") &&
                ((xt = Lf),
                (nt = "onPointerLeave"),
                (U = "onPointerEnter"),
                (C = "pointer")),
              (qt = K == null ? V : Ul(K)),
              (q = mt == null ? V : Ul(mt)),
              (V = new xt(nt, C + "leave", K, n, P)),
              (V.target = qt),
              (V.relatedTarget = q),
              (nt = null),
              el(P) === X &&
                ((xt = new xt(U, C + "enter", mt, n, P)),
                (xt.target = q),
                (xt.relatedTarget = qt),
                (nt = xt)),
              (qt = nt),
              K && mt)
            )
              e: {
                for (xt = wv, U = K, C = mt, q = 0, nt = U; nt; nt = xt(nt))
                  q++;
                nt = 0;
                for (var vt = C; vt; vt = xt(vt)) nt++;
                for (; 0 < q - nt; ) ((U = xt(U)), q--);
                for (; 0 < nt - q; ) ((C = xt(C)), nt--);
                for (; q--; ) {
                  if (U === C || (C !== null && U === C.alternate)) {
                    xt = U;
                    break e;
                  }
                  ((U = xt(U)), (C = xt(C)));
                }
                xt = null;
              }
            else xt = null;
            (K !== null && Rm(lt, V, K, xt, !1),
              mt !== null && qt !== null && Rm(lt, qt, mt, xt, !0));
          }
        }
        t: {
          if (
            ((V = X ? Ul(X) : window),
            (K = V.nodeName && V.nodeName.toLowerCase()),
            K === "select" || (K === "input" && V.type === "file"))
          )
            var Ot = Wf;
          else if ($f(V))
            if (Ff) Ot = jp;
            else {
              Ot = Rp;
              var yt = Op;
            }
          else
            ((K = V.nodeName),
              !K ||
              K.toLowerCase() !== "input" ||
              (V.type !== "checkbox" && V.type !== "radio")
                ? X && Es(X.elementType) && (Ot = Wf)
                : (Ot = Hp));
          if (Ot && (Ot = Ot(t, X))) {
            Jf(lt, Ot, n, P);
            break t;
          }
          (yt && yt(t, V, X),
            t === "focusout" &&
              X &&
              V.type === "number" &&
              X.memoizedProps.value != null &&
              Ss(V, "number", V.value));
        }
        switch (((yt = X ? Ul(X) : window), t)) {
          case "focusin":
            ($f(yt) || yt.contentEditable === "true") &&
              ((da = yt), (Us = X), (mu = null));
            break;
          case "focusout":
            mu = Us = da = null;
            break;
          case "mousedown":
            Bs = !0;
            break;
          case "contextmenu":
          case "mouseup":
          case "dragend":
            ((Bs = !1), ud(lt, n, P));
            break;
          case "selectionchange":
            if (Bp) break;
          case "keydown":
          case "keyup":
            ud(lt, n, P);
        }
        var _t;
        if (Os)
          t: {
            switch (t) {
              case "compositionstart":
                var Mt = "onCompositionStart";
                break t;
              case "compositionend":
                Mt = "onCompositionEnd";
                break t;
              case "compositionupdate":
                Mt = "onCompositionUpdate";
                break t;
            }
            Mt = void 0;
          }
        else
          fa
            ? kf(t, n) && (Mt = "onCompositionEnd")
            : t === "keydown" &&
              n.keyCode === 229 &&
              (Mt = "onCompositionStart");
        (Mt &&
          (Gf &&
            n.locale !== "ko" &&
            (fa || Mt !== "onCompositionStart"
              ? Mt === "onCompositionEnd" && fa && (_t = Yf())
              : ((ll = P),
                (Ts = "value" in ll ? ll.value : ll.textContent),
                (fa = !0))),
          (yt = _c(X, Mt)),
          0 < yt.length &&
            ((Mt = new Vf(Mt, t, null, n, P)),
            lt.push({ event: Mt, listeners: yt }),
            _t
              ? (Mt.data = _t)
              : ((_t = Kf(n)), _t !== null && (Mt.data = _t)))),
          (_t = Tp ? zp(t, n) : Mp(t, n)) &&
            ((Mt = _c(X, "onBeforeInput")),
            0 < Mt.length &&
              ((yt = new Vf("onBeforeInput", "beforeinput", null, n, P)),
              lt.push({ event: yt, listeners: Mt }),
              (yt.data = _t))),
          bv(lt, t, X, n, P));
      }
      Dm(lt, e);
    });
  }
  function qu(t, e, n) {
    return { instance: t, listener: e, currentTarget: n };
  }
  function _c(t, e) {
    for (var n = e + "Capture", a = []; t !== null; ) {
      var c = t,
        r = c.stateNode;
      if (
        ((c = c.tag),
        (c !== 5 && c !== 26 && c !== 27) ||
          r === null ||
          ((c = iu(t, n)),
          c != null && a.unshift(qu(t, c, r)),
          (c = iu(t, e)),
          c != null && a.push(qu(t, c, r))),
        t.tag === 3)
      )
        return a;
      t = t.return;
    }
    return [];
  }
  function wv(t) {
    if (t === null) return null;
    do t = t.return;
    while (t && t.tag !== 5 && t.tag !== 27);
    return t || null;
  }
  function Rm(t, e, n, a, c) {
    for (var r = e._reactName, h = []; n !== null && n !== a; ) {
      var v = n,
        T = v.alternate,
        X = v.stateNode;
      if (((v = v.tag), T !== null && T === a)) break;
      ((v !== 5 && v !== 26 && v !== 27) ||
        X === null ||
        ((T = X),
        c
          ? ((X = iu(n, r)), X != null && h.unshift(qu(n, X, T)))
          : c || ((X = iu(n, r)), X != null && h.push(qu(n, X, T)))),
        (n = n.return));
    }
    h.length !== 0 && t.push({ event: e, listeners: h });
  }
  var Nv = /\r\n?/g,
    Av = /\u0000|\uFFFD/g;
  function Hm(t) {
    return (typeof t == "string" ? t : "" + t)
      .replace(
        Nv,
        `
`,
      )
      .replace(Av, "");
  }
  function jm(t, e) {
    return ((e = Hm(e)), Hm(t) === e);
  }
  function Yt(t, e, n, a, c, r) {
    switch (n) {
      case "children":
        typeof a == "string"
          ? e === "body" || (e === "textarea" && a === "") || sa(t, a)
          : (typeof a == "number" || typeof a == "bigint") &&
            e !== "body" &&
            sa(t, "" + a);
        break;
      case "className":
        Ti(t, "class", a);
        break;
      case "tabIndex":
        Ti(t, "tabindex", a);
        break;
      case "dir":
      case "role":
      case "viewBox":
      case "width":
      case "height":
        Ti(t, n, a);
        break;
      case "style":
        jf(t, a, r);
        break;
      case "data":
        if (e !== "object") {
          Ti(t, "data", a);
          break;
        }
      case "src":
      case "href":
        if (a === "" && (e !== "a" || n !== "href")) {
          t.removeAttribute(n);
          break;
        }
        if (
          a == null ||
          typeof a == "function" ||
          typeof a == "symbol" ||
          typeof a == "boolean"
        ) {
          t.removeAttribute(n);
          break;
        }
        ((a = Mi("" + a)), t.setAttribute(n, a));
        break;
      case "action":
      case "formAction":
        if (typeof a == "function") {
          t.setAttribute(
            n,
            "javascript:throw new Error('A React form was unexpectedly submitted. If you called form.submit() manually, consider using form.requestSubmit() instead. If you\\'re trying to use event.stopPropagation() in a submit event handler, consider also calling event.preventDefault().')",
          );
          break;
        } else
          typeof r == "function" &&
            (n === "formAction"
              ? (e !== "input" && Yt(t, e, "name", c.name, c, null),
                Yt(t, e, "formEncType", c.formEncType, c, null),
                Yt(t, e, "formMethod", c.formMethod, c, null),
                Yt(t, e, "formTarget", c.formTarget, c, null))
              : (Yt(t, e, "encType", c.encType, c, null),
                Yt(t, e, "method", c.method, c, null),
                Yt(t, e, "target", c.target, c, null)));
        if (a == null || typeof a == "symbol" || typeof a == "boolean") {
          t.removeAttribute(n);
          break;
        }
        ((a = Mi("" + a)), t.setAttribute(n, a));
        break;
      case "onClick":
        a != null && (t.onclick = An);
        break;
      case "onScroll":
        a != null && Tt("scroll", t);
        break;
      case "onScrollEnd":
        a != null && Tt("scrollend", t);
        break;
      case "dangerouslySetInnerHTML":
        if (a != null) {
          if (typeof a != "object" || !("__html" in a)) throw Error(s(61));
          if (((n = a.__html), n != null)) {
            if (c.children != null) throw Error(s(60));
            t.innerHTML = n;
          }
        }
        break;
      case "multiple":
        t.multiple = a && typeof a != "function" && typeof a != "symbol";
        break;
      case "muted":
        t.muted = a && typeof a != "function" && typeof a != "symbol";
        break;
      case "suppressContentEditableWarning":
      case "suppressHydrationWarning":
      case "defaultValue":
      case "defaultChecked":
      case "innerHTML":
      case "ref":
        break;
      case "autoFocus":
        break;
      case "xlinkHref":
        if (
          a == null ||
          typeof a == "function" ||
          typeof a == "boolean" ||
          typeof a == "symbol"
        ) {
          t.removeAttribute("xlink:href");
          break;
        }
        ((n = Mi("" + a)),
          t.setAttributeNS("http://www.w3.org/1999/xlink", "xlink:href", n));
        break;
      case "contentEditable":
      case "spellCheck":
      case "draggable":
      case "value":
      case "autoReverse":
      case "externalResourcesRequired":
      case "focusable":
      case "preserveAlpha":
        a != null && typeof a != "function" && typeof a != "symbol"
          ? t.setAttribute(n, "" + a)
          : t.removeAttribute(n);
        break;
      case "inert":
      case "allowFullScreen":
      case "async":
      case "autoPlay":
      case "controls":
      case "default":
      case "defer":
      case "disabled":
      case "disablePictureInPicture":
      case "disableRemotePlayback":
      case "formNoValidate":
      case "hidden":
      case "loop":
      case "noModule":
      case "noValidate":
      case "open":
      case "playsInline":
      case "readOnly":
      case "required":
      case "reversed":
      case "scoped":
      case "seamless":
      case "itemScope":
        a && typeof a != "function" && typeof a != "symbol"
          ? t.setAttribute(n, "")
          : t.removeAttribute(n);
        break;
      case "capture":
      case "download":
        a === !0
          ? t.setAttribute(n, "")
          : a !== !1 &&
              a != null &&
              typeof a != "function" &&
              typeof a != "symbol"
            ? t.setAttribute(n, a)
            : t.removeAttribute(n);
        break;
      case "cols":
      case "rows":
      case "size":
      case "span":
        a != null &&
        typeof a != "function" &&
        typeof a != "symbol" &&
        !isNaN(a) &&
        1 <= a
          ? t.setAttribute(n, a)
          : t.removeAttribute(n);
        break;
      case "rowSpan":
      case "start":
        a == null || typeof a == "function" || typeof a == "symbol" || isNaN(a)
          ? t.removeAttribute(n)
          : t.setAttribute(n, a);
        break;
      case "popover":
        (Tt("beforetoggle", t), Tt("toggle", t), Ai(t, "popover", a));
        break;
      case "xlinkActuate":
        Nn(t, "http://www.w3.org/1999/xlink", "xlink:actuate", a);
        break;
      case "xlinkArcrole":
        Nn(t, "http://www.w3.org/1999/xlink", "xlink:arcrole", a);
        break;
      case "xlinkRole":
        Nn(t, "http://www.w3.org/1999/xlink", "xlink:role", a);
        break;
      case "xlinkShow":
        Nn(t, "http://www.w3.org/1999/xlink", "xlink:show", a);
        break;
      case "xlinkTitle":
        Nn(t, "http://www.w3.org/1999/xlink", "xlink:title", a);
        break;
      case "xlinkType":
        Nn(t, "http://www.w3.org/1999/xlink", "xlink:type", a);
        break;
      case "xmlBase":
        Nn(t, "http://www.w3.org/XML/1998/namespace", "xml:base", a);
        break;
      case "xmlLang":
        Nn(t, "http://www.w3.org/XML/1998/namespace", "xml:lang", a);
        break;
      case "xmlSpace":
        Nn(t, "http://www.w3.org/XML/1998/namespace", "xml:space", a);
        break;
      case "is":
        Ai(t, "is", a);
        break;
      case "innerText":
      case "textContent":
        break;
      default:
        (!(2 < n.length) ||
          (n[0] !== "o" && n[0] !== "O") ||
          (n[1] !== "n" && n[1] !== "N")) &&
          ((n = tp.get(n) || n), Ai(t, n, a));
    }
  }
  function so(t, e, n, a, c, r) {
    switch (n) {
      case "style":
        jf(t, a, r);
        break;
      case "dangerouslySetInnerHTML":
        if (a != null) {
          if (typeof a != "object" || !("__html" in a)) throw Error(s(61));
          if (((n = a.__html), n != null)) {
            if (c.children != null) throw Error(s(60));
            t.innerHTML = n;
          }
        }
        break;
      case "children":
        typeof a == "string"
          ? sa(t, a)
          : (typeof a == "number" || typeof a == "bigint") && sa(t, "" + a);
        break;
      case "onScroll":
        a != null && Tt("scroll", t);
        break;
      case "onScrollEnd":
        a != null && Tt("scrollend", t);
        break;
      case "onClick":
        a != null && (t.onclick = An);
        break;
      case "suppressContentEditableWarning":
      case "suppressHydrationWarning":
      case "innerHTML":
      case "ref":
        break;
      case "innerText":
      case "textContent":
        break;
      default:
        if (!_i.hasOwnProperty(n))
          t: {
            if (
              n[0] === "o" &&
              n[1] === "n" &&
              ((c = n.endsWith("Capture")),
              (e = n.slice(2, c ? n.length - 7 : void 0)),
              (r = t[ge] || null),
              (r = r != null ? r[n] : null),
              typeof r == "function" && t.removeEventListener(e, r, c),
              typeof a == "function")
            ) {
              (typeof r != "function" &&
                r !== null &&
                (n in t
                  ? (t[n] = null)
                  : t.hasAttribute(n) && t.removeAttribute(n)),
                t.addEventListener(e, a, c));
              break t;
            }
            n in t
              ? (t[n] = a)
              : a === !0
                ? t.setAttribute(n, "")
                : Ai(t, n, a);
          }
    }
  }
  function xe(t, e, n) {
    switch (e) {
      case "div":
      case "span":
      case "svg":
      case "path":
      case "a":
      case "g":
      case "p":
      case "li":
        break;
      case "img":
        (Tt("error", t), Tt("load", t));
        var a = !1,
          c = !1,
          r;
        for (r in n)
          if (n.hasOwnProperty(r)) {
            var h = n[r];
            if (h != null)
              switch (r) {
                case "src":
                  a = !0;
                  break;
                case "srcSet":
                  c = !0;
                  break;
                case "children":
                case "dangerouslySetInnerHTML":
                  throw Error(s(137, e));
                default:
                  Yt(t, e, r, h, n, null);
              }
          }
        (c && Yt(t, e, "srcSet", n.srcSet, n, null),
          a && Yt(t, e, "src", n.src, n, null));
        return;
      case "input":
        Tt("invalid", t);
        var v = (r = h = c = null),
          T = null,
          X = null;
        for (a in n)
          if (n.hasOwnProperty(a)) {
            var P = n[a];
            if (P != null)
              switch (a) {
                case "name":
                  c = P;
                  break;
                case "type":
                  h = P;
                  break;
                case "checked":
                  T = P;
                  break;
                case "defaultChecked":
                  X = P;
                  break;
                case "value":
                  r = P;
                  break;
                case "defaultValue":
                  v = P;
                  break;
                case "children":
                case "dangerouslySetInnerHTML":
                  if (P != null) throw Error(s(137, e));
                  break;
                default:
                  Yt(t, e, a, P, n, null);
              }
          }
        Df(t, r, v, T, X, h, c, !1);
        return;
      case "select":
        (Tt("invalid", t), (a = h = r = null));
        for (c in n)
          if (n.hasOwnProperty(c) && ((v = n[c]), v != null))
            switch (c) {
              case "value":
                r = v;
                break;
              case "defaultValue":
                h = v;
                break;
              case "multiple":
                a = v;
              default:
                Yt(t, e, c, v, n, null);
            }
        ((e = r),
          (n = h),
          (t.multiple = !!a),
          e != null ? ca(t, !!a, e, !1) : n != null && ca(t, !!a, n, !0));
        return;
      case "textarea":
        (Tt("invalid", t), (r = c = a = null));
        for (h in n)
          if (n.hasOwnProperty(h) && ((v = n[h]), v != null))
            switch (h) {
              case "value":
                a = v;
                break;
              case "defaultValue":
                c = v;
                break;
              case "children":
                r = v;
                break;
              case "dangerouslySetInnerHTML":
                if (v != null) throw Error(s(91));
                break;
              default:
                Yt(t, e, h, v, n, null);
            }
        Rf(t, a, c, r);
        return;
      case "option":
        for (T in n)
          if (n.hasOwnProperty(T) && ((a = n[T]), a != null))
            switch (T) {
              case "selected":
                t.selected =
                  a && typeof a != "function" && typeof a != "symbol";
                break;
              default:
                Yt(t, e, T, a, n, null);
            }
        return;
      case "dialog":
        (Tt("beforetoggle", t),
          Tt("toggle", t),
          Tt("cancel", t),
          Tt("close", t));
        break;
      case "iframe":
      case "object":
        Tt("load", t);
        break;
      case "video":
      case "audio":
        for (a = 0; a < Yu.length; a++) Tt(Yu[a], t);
        break;
      case "image":
        (Tt("error", t), Tt("load", t));
        break;
      case "details":
        Tt("toggle", t);
        break;
      case "embed":
      case "source":
      case "link":
        (Tt("error", t), Tt("load", t));
      case "area":
      case "base":
      case "br":
      case "col":
      case "hr":
      case "keygen":
      case "meta":
      case "param":
      case "track":
      case "wbr":
      case "menuitem":
        for (X in n)
          if (n.hasOwnProperty(X) && ((a = n[X]), a != null))
            switch (X) {
              case "children":
              case "dangerouslySetInnerHTML":
                throw Error(s(137, e));
              default:
                Yt(t, e, X, a, n, null);
            }
        return;
      default:
        if (Es(e)) {
          for (P in n)
            n.hasOwnProperty(P) &&
              ((a = n[P]), a !== void 0 && so(t, e, P, a, n, void 0));
          return;
        }
    }
    for (v in n)
      n.hasOwnProperty(v) && ((a = n[v]), a != null && Yt(t, e, v, a, n, null));
  }
  function Tv(t, e, n, a) {
    switch (e) {
      case "div":
      case "span":
      case "svg":
      case "path":
      case "a":
      case "g":
      case "p":
      case "li":
        break;
      case "input":
        var c = null,
          r = null,
          h = null,
          v = null,
          T = null,
          X = null,
          P = null;
        for (K in n) {
          var lt = n[K];
          if (n.hasOwnProperty(K) && lt != null)
            switch (K) {
              case "checked":
                break;
              case "value":
                break;
              case "defaultValue":
                T = lt;
              default:
                a.hasOwnProperty(K) || Yt(t, e, K, null, a, lt);
            }
        }
        for (var V in a) {
          var K = a[V];
          if (((lt = n[V]), a.hasOwnProperty(V) && (K != null || lt != null)))
            switch (V) {
              case "type":
                r = K;
                break;
              case "name":
                c = K;
                break;
              case "checked":
                X = K;
                break;
              case "defaultChecked":
                P = K;
                break;
              case "value":
                h = K;
                break;
              case "defaultValue":
                v = K;
                break;
              case "children":
              case "dangerouslySetInnerHTML":
                if (K != null) throw Error(s(137, e));
                break;
              default:
                K !== lt && Yt(t, e, V, K, a, lt);
            }
        }
        bs(t, h, v, T, X, P, r, c);
        return;
      case "select":
        K = h = v = V = null;
        for (r in n)
          if (((T = n[r]), n.hasOwnProperty(r) && T != null))
            switch (r) {
              case "value":
                break;
              case "multiple":
                K = T;
              default:
                a.hasOwnProperty(r) || Yt(t, e, r, null, a, T);
            }
        for (c in a)
          if (
            ((r = a[c]),
            (T = n[c]),
            a.hasOwnProperty(c) && (r != null || T != null))
          )
            switch (c) {
              case "value":
                V = r;
                break;
              case "defaultValue":
                v = r;
                break;
              case "multiple":
                h = r;
              default:
                r !== T && Yt(t, e, c, r, a, T);
            }
        ((e = v),
          (n = h),
          (a = K),
          V != null
            ? ca(t, !!n, V, !1)
            : !!a != !!n &&
              (e != null ? ca(t, !!n, e, !0) : ca(t, !!n, n ? [] : "", !1)));
        return;
      case "textarea":
        K = V = null;
        for (v in n)
          if (
            ((c = n[v]),
            n.hasOwnProperty(v) && c != null && !a.hasOwnProperty(v))
          )
            switch (v) {
              case "value":
                break;
              case "children":
                break;
              default:
                Yt(t, e, v, null, a, c);
            }
        for (h in a)
          if (
            ((c = a[h]),
            (r = n[h]),
            a.hasOwnProperty(h) && (c != null || r != null))
          )
            switch (h) {
              case "value":
                V = c;
                break;
              case "defaultValue":
                K = c;
                break;
              case "children":
                break;
              case "dangerouslySetInnerHTML":
                if (c != null) throw Error(s(91));
                break;
              default:
                c !== r && Yt(t, e, h, c, a, r);
            }
        Of(t, V, K);
        return;
      case "option":
        for (var mt in n)
          if (
            ((V = n[mt]),
            n.hasOwnProperty(mt) && V != null && !a.hasOwnProperty(mt))
          )
            switch (mt) {
              case "selected":
                t.selected = !1;
                break;
              default:
                Yt(t, e, mt, null, a, V);
            }
        for (T in a)
          if (
            ((V = a[T]),
            (K = n[T]),
            a.hasOwnProperty(T) && V !== K && (V != null || K != null))
          )
            switch (T) {
              case "selected":
                t.selected =
                  V && typeof V != "function" && typeof V != "symbol";
                break;
              default:
                Yt(t, e, T, V, a, K);
            }
        return;
      case "img":
      case "link":
      case "area":
      case "base":
      case "br":
      case "col":
      case "embed":
      case "hr":
      case "keygen":
      case "meta":
      case "param":
      case "source":
      case "track":
      case "wbr":
      case "menuitem":
        for (var xt in n)
          ((V = n[xt]),
            n.hasOwnProperty(xt) &&
              V != null &&
              !a.hasOwnProperty(xt) &&
              Yt(t, e, xt, null, a, V));
        for (X in a)
          if (
            ((V = a[X]),
            (K = n[X]),
            a.hasOwnProperty(X) && V !== K && (V != null || K != null))
          )
            switch (X) {
              case "children":
              case "dangerouslySetInnerHTML":
                if (V != null) throw Error(s(137, e));
                break;
              default:
                Yt(t, e, X, V, a, K);
            }
        return;
      default:
        if (Es(e)) {
          for (var qt in n)
            ((V = n[qt]),
              n.hasOwnProperty(qt) &&
                V !== void 0 &&
                !a.hasOwnProperty(qt) &&
                so(t, e, qt, void 0, a, V));
          for (P in a)
            ((V = a[P]),
              (K = n[P]),
              !a.hasOwnProperty(P) ||
                V === K ||
                (V === void 0 && K === void 0) ||
                so(t, e, P, V, a, K));
          return;
        }
    }
    for (var U in n)
      ((V = n[U]),
        n.hasOwnProperty(U) &&
          V != null &&
          !a.hasOwnProperty(U) &&
          Yt(t, e, U, null, a, V));
    for (lt in a)
      ((V = a[lt]),
        (K = n[lt]),
        !a.hasOwnProperty(lt) ||
          V === K ||
          (V == null && K == null) ||
          Yt(t, e, lt, V, a, K));
  }
  function Um(t) {
    switch (t) {
      case "css":
      case "script":
      case "font":
      case "img":
      case "image":
      case "input":
      case "link":
        return !0;
      default:
        return !1;
    }
  }
  function zv() {
    if (typeof performance.getEntriesByType == "function") {
      for (
        var t = 0, e = 0, n = performance.getEntriesByType("resource"), a = 0;
        a < n.length;
        a++
      ) {
        var c = n[a],
          r = c.transferSize,
          h = c.initiatorType,
          v = c.duration;
        if (r && v && Um(h)) {
          for (h = 0, v = c.responseEnd, a += 1; a < n.length; a++) {
            var T = n[a],
              X = T.startTime;
            if (X > v) break;
            var P = T.transferSize,
              lt = T.initiatorType;
            P &&
              Um(lt) &&
              ((T = T.responseEnd), (h += P * (T < v ? 1 : (v - X) / (T - X))));
          }
          if ((--a, (e += (8 * (r + h)) / (c.duration / 1e3)), t++, 10 < t))
            break;
        }
      }
      if (0 < t) return e / t / 1e6;
    }
    return navigator.connection &&
      ((t = navigator.connection.downlink), typeof t == "number")
      ? t
      : 5;
  }
  var ro = null,
    oo = null;
  function wc(t) {
    return t.nodeType === 9 ? t : t.ownerDocument;
  }
  function Bm(t) {
    switch (t) {
      case "http://www.w3.org/2000/svg":
        return 1;
      case "http://www.w3.org/1998/Math/MathML":
        return 2;
      default:
        return 0;
    }
  }
  function Ym(t, e) {
    if (t === 0)
      switch (e) {
        case "svg":
          return 1;
        case "math":
          return 2;
        default:
          return 0;
      }
    return t === 1 && e === "foreignObject" ? 0 : t;
  }
  function fo(t, e) {
    return (
      t === "textarea" ||
      t === "noscript" ||
      typeof e.children == "string" ||
      typeof e.children == "number" ||
      typeof e.children == "bigint" ||
      (typeof e.dangerouslySetInnerHTML == "object" &&
        e.dangerouslySetInnerHTML !== null &&
        e.dangerouslySetInnerHTML.__html != null)
    );
  }
  var ho = null;
  function Mv() {
    var t = window.event;
    return t && t.type === "popstate"
      ? t === ho
        ? !1
        : ((ho = t), !0)
      : ((ho = null), !1);
  }
  var qm = typeof setTimeout == "function" ? setTimeout : void 0,
    Cv = typeof clearTimeout == "function" ? clearTimeout : void 0,
    Xm = typeof Promise == "function" ? Promise : void 0,
    Dv =
      typeof queueMicrotask == "function"
        ? queueMicrotask
        : typeof Xm < "u"
          ? function (t) {
              return Xm.resolve(null).then(t).catch(Ov);
            }
          : qm;
  function Ov(t) {
    setTimeout(function () {
      throw t;
    });
  }
  function bl(t) {
    return t === "head";
  }
  function Vm(t, e) {
    var n = e,
      a = 0;
    do {
      var c = n.nextSibling;
      if ((t.removeChild(n), c && c.nodeType === 8))
        if (((n = c.data), n === "/$" || n === "/&")) {
          if (a === 0) {
            (t.removeChild(c), qa(e));
            return;
          }
          a--;
        } else if (
          n === "$" ||
          n === "$?" ||
          n === "$~" ||
          n === "$!" ||
          n === "&"
        )
          a++;
        else if (n === "html") Xu(t.ownerDocument.documentElement);
        else if (n === "head") {
          ((n = t.ownerDocument.head), Xu(n));
          for (var r = n.firstChild; r; ) {
            var h = r.nextSibling,
              v = r.nodeName;
            (r[jl] ||
              v === "SCRIPT" ||
              v === "STYLE" ||
              (v === "LINK" && r.rel.toLowerCase() === "stylesheet") ||
              n.removeChild(r),
              (r = h));
          }
        } else n === "body" && Xu(t.ownerDocument.body);
      n = c;
    } while (n);
    qa(e);
  }
  function Lm(t, e) {
    var n = t;
    t = 0;
    do {
      var a = n.nextSibling;
      if (
        (n.nodeType === 1
          ? e
            ? ((n._stashedDisplay = n.style.display),
              (n.style.display = "none"))
            : ((n.style.display = n._stashedDisplay || ""),
              n.getAttribute("style") === "" && n.removeAttribute("style"))
          : n.nodeType === 3 &&
            (e
              ? ((n._stashedText = n.nodeValue), (n.nodeValue = ""))
              : (n.nodeValue = n._stashedText || "")),
        a && a.nodeType === 8)
      )
        if (((n = a.data), n === "/$")) {
          if (t === 0) break;
          t--;
        } else (n !== "$" && n !== "$?" && n !== "$~" && n !== "$!") || t++;
      n = a;
    } while (n);
  }
  function mo(t) {
    var e = t.firstChild;
    for (e && e.nodeType === 10 && (e = e.nextSibling); e; ) {
      var n = e;
      switch (((e = e.nextSibling), n.nodeName)) {
        case "HTML":
        case "HEAD":
        case "BODY":
          (mo(n), au(n));
          continue;
        case "SCRIPT":
        case "STYLE":
          continue;
        case "LINK":
          if (n.rel.toLowerCase() === "stylesheet") continue;
      }
      t.removeChild(n);
    }
  }
  function Rv(t, e, n, a) {
    for (; t.nodeType === 1; ) {
      var c = n;
      if (t.nodeName.toLowerCase() !== e.toLowerCase()) {
        if (!a && (t.nodeName !== "INPUT" || t.type !== "hidden")) break;
      } else if (a) {
        if (!t[jl])
          switch (e) {
            case "meta":
              if (!t.hasAttribute("itemprop")) break;
              return t;
            case "link":
              if (
                ((r = t.getAttribute("rel")),
                r === "stylesheet" && t.hasAttribute("data-precedence"))
              )
                break;
              if (
                r !== c.rel ||
                t.getAttribute("href") !==
                  (c.href == null || c.href === "" ? null : c.href) ||
                t.getAttribute("crossorigin") !==
                  (c.crossOrigin == null ? null : c.crossOrigin) ||
                t.getAttribute("title") !== (c.title == null ? null : c.title)
              )
                break;
              return t;
            case "style":
              if (t.hasAttribute("data-precedence")) break;
              return t;
            case "script":
              if (
                ((r = t.getAttribute("src")),
                (r !== (c.src == null ? null : c.src) ||
                  t.getAttribute("type") !== (c.type == null ? null : c.type) ||
                  t.getAttribute("crossorigin") !==
                    (c.crossOrigin == null ? null : c.crossOrigin)) &&
                  r &&
                  t.hasAttribute("async") &&
                  !t.hasAttribute("itemprop"))
              )
                break;
              return t;
            default:
              return t;
          }
      } else if (e === "input" && t.type === "hidden") {
        var r = c.name == null ? null : "" + c.name;
        if (c.type === "hidden" && t.getAttribute("name") === r) return t;
      } else return t;
      if (((t = Ie(t.nextSibling)), t === null)) break;
    }
    return null;
  }
  function Hv(t, e, n) {
    if (e === "") return null;
    for (; t.nodeType !== 3; )
      if (
        ((t.nodeType !== 1 || t.nodeName !== "INPUT" || t.type !== "hidden") &&
          !n) ||
        ((t = Ie(t.nextSibling)), t === null)
      )
        return null;
    return t;
  }
  function Gm(t, e) {
    for (; t.nodeType !== 8; )
      if (
        ((t.nodeType !== 1 || t.nodeName !== "INPUT" || t.type !== "hidden") &&
          !e) ||
        ((t = Ie(t.nextSibling)), t === null)
      )
        return null;
    return t;
  }
  function go(t) {
    return t.data === "$?" || t.data === "$~";
  }
  function yo(t) {
    return (
      t.data === "$!" ||
      (t.data === "$?" && t.ownerDocument.readyState !== "loading")
    );
  }
  function jv(t, e) {
    var n = t.ownerDocument;
    if (t.data === "$~") t._reactRetry = e;
    else if (t.data !== "$?" || n.readyState !== "loading") e();
    else {
      var a = function () {
        (e(), n.removeEventListener("DOMContentLoaded", a));
      };
      (n.addEventListener("DOMContentLoaded", a), (t._reactRetry = a));
    }
  }
  function Ie(t) {
    for (; t != null; t = t.nextSibling) {
      var e = t.nodeType;
      if (e === 1 || e === 3) break;
      if (e === 8) {
        if (
          ((e = t.data),
          e === "$" ||
            e === "$!" ||
            e === "$?" ||
            e === "$~" ||
            e === "&" ||
            e === "F!" ||
            e === "F")
        )
          break;
        if (e === "/$" || e === "/&") return null;
      }
    }
    return t;
  }
  var po = null;
  function Zm(t) {
    t = t.nextSibling;
    for (var e = 0; t; ) {
      if (t.nodeType === 8) {
        var n = t.data;
        if (n === "/$" || n === "/&") {
          if (e === 0) return Ie(t.nextSibling);
          e--;
        } else
          (n !== "$" && n !== "$!" && n !== "$?" && n !== "$~" && n !== "&") ||
            e++;
      }
      t = t.nextSibling;
    }
    return null;
  }
  function Qm(t) {
    t = t.previousSibling;
    for (var e = 0; t; ) {
      if (t.nodeType === 8) {
        var n = t.data;
        if (n === "$" || n === "$!" || n === "$?" || n === "$~" || n === "&") {
          if (e === 0) return t;
          e--;
        } else (n !== "/$" && n !== "/&") || e++;
      }
      t = t.previousSibling;
    }
    return null;
  }
  function km(t, e, n) {
    switch (((e = wc(n)), t)) {
      case "html":
        if (((t = e.documentElement), !t)) throw Error(s(452));
        return t;
      case "head":
        if (((t = e.head), !t)) throw Error(s(453));
        return t;
      case "body":
        if (((t = e.body), !t)) throw Error(s(454));
        return t;
      default:
        throw Error(s(451));
    }
  }
  function Xu(t) {
    for (var e = t.attributes; e.length; ) t.removeAttributeNode(e[0]);
    au(t);
  }
  var Pe = new Map(),
    Km = new Set();
  function Nc(t) {
    return typeof t.getRootNode == "function"
      ? t.getRootNode()
      : t.nodeType === 9
        ? t
        : t.ownerDocument;
  }
  var Ln = R.d;
  R.d = { f: Uv, r: Bv, D: Yv, C: qv, L: Xv, m: Vv, X: Gv, S: Lv, M: Zv };
  function Uv() {
    var t = Ln.f(),
      e = yc();
    return t || e;
  }
  function Bv(t) {
    var e = En(t);
    e !== null && e.tag === 5 && e.type === "form" ? oh(e) : Ln.r(t);
  }
  var Ua = typeof document > "u" ? null : document;
  function $m(t, e, n) {
    var a = Ua;
    if (a && typeof e == "string" && e) {
      var c = Qe(e);
      ((c = 'link[rel="' + t + '"][href="' + c + '"]'),
        typeof n == "string" && (c += '[crossorigin="' + n + '"]'),
        Km.has(c) ||
          (Km.add(c),
          (t = { rel: t, crossOrigin: n, href: e }),
          a.querySelector(c) === null &&
            ((e = a.createElement("link")),
            xe(e, "link", t),
            le(e),
            a.head.appendChild(e))));
    }
  }
  function Yv(t) {
    (Ln.D(t), $m("dns-prefetch", t, null));
  }
  function qv(t, e) {
    (Ln.C(t, e), $m("preconnect", t, e));
  }
  function Xv(t, e, n) {
    Ln.L(t, e, n);
    var a = Ua;
    if (a && t && e) {
      var c = 'link[rel="preload"][as="' + Qe(e) + '"]';
      e === "image" && n && n.imageSrcSet
        ? ((c += '[imagesrcset="' + Qe(n.imageSrcSet) + '"]'),
          typeof n.imageSizes == "string" &&
            (c += '[imagesizes="' + Qe(n.imageSizes) + '"]'))
        : (c += '[href="' + Qe(t) + '"]');
      var r = c;
      switch (e) {
        case "style":
          r = Ba(t);
          break;
        case "script":
          r = Ya(t);
      }
      Pe.has(r) ||
        ((t = x(
          {
            rel: "preload",
            href: e === "image" && n && n.imageSrcSet ? void 0 : t,
            as: e,
          },
          n,
        )),
        Pe.set(r, t),
        a.querySelector(c) !== null ||
          (e === "style" && a.querySelector(Vu(r))) ||
          (e === "script" && a.querySelector(Lu(r))) ||
          ((e = a.createElement("link")),
          xe(e, "link", t),
          le(e),
          a.head.appendChild(e)));
    }
  }
  function Vv(t, e) {
    Ln.m(t, e);
    var n = Ua;
    if (n && t) {
      var a = e && typeof e.as == "string" ? e.as : "script",
        c =
          'link[rel="modulepreload"][as="' + Qe(a) + '"][href="' + Qe(t) + '"]',
        r = c;
      switch (a) {
        case "audioworklet":
        case "paintworklet":
        case "serviceworker":
        case "sharedworker":
        case "worker":
        case "script":
          r = Ya(t);
      }
      if (
        !Pe.has(r) &&
        ((t = x({ rel: "modulepreload", href: t }, e)),
        Pe.set(r, t),
        n.querySelector(c) === null)
      ) {
        switch (a) {
          case "audioworklet":
          case "paintworklet":
          case "serviceworker":
          case "sharedworker":
          case "worker":
          case "script":
            if (n.querySelector(Lu(r))) return;
        }
        ((a = n.createElement("link")),
          xe(a, "link", t),
          le(a),
          n.head.appendChild(a));
      }
    }
  }
  function Lv(t, e, n) {
    Ln.S(t, e, n);
    var a = Ua;
    if (a && t) {
      var c = nl(a).hoistableStyles,
        r = Ba(t);
      e = e || "default";
      var h = c.get(r);
      if (!h) {
        var v = { loading: 0, preload: null };
        if ((h = a.querySelector(Vu(r)))) v.loading = 5;
        else {
          ((t = x({ rel: "stylesheet", href: t, "data-precedence": e }, n)),
            (n = Pe.get(r)) && vo(t, n));
          var T = (h = a.createElement("link"));
          (le(T),
            xe(T, "link", t),
            (T._p = new Promise(function (X, P) {
              ((T.onload = X), (T.onerror = P));
            })),
            T.addEventListener("load", function () {
              v.loading |= 1;
            }),
            T.addEventListener("error", function () {
              v.loading |= 2;
            }),
            (v.loading |= 4),
            Ac(h, e, a));
        }
        ((h = { type: "stylesheet", instance: h, count: 1, state: v }),
          c.set(r, h));
      }
    }
  }
  function Gv(t, e) {
    Ln.X(t, e);
    var n = Ua;
    if (n && t) {
      var a = nl(n).hoistableScripts,
        c = Ya(t),
        r = a.get(c);
      r ||
        ((r = n.querySelector(Lu(c))),
        r ||
          ((t = x({ src: t, async: !0 }, e)),
          (e = Pe.get(c)) && xo(t, e),
          (r = n.createElement("script")),
          le(r),
          xe(r, "link", t),
          n.head.appendChild(r)),
        (r = { type: "script", instance: r, count: 1, state: null }),
        a.set(c, r));
    }
  }
  function Zv(t, e) {
    Ln.M(t, e);
    var n = Ua;
    if (n && t) {
      var a = nl(n).hoistableScripts,
        c = Ya(t),
        r = a.get(c);
      r ||
        ((r = n.querySelector(Lu(c))),
        r ||
          ((t = x({ src: t, async: !0, type: "module" }, e)),
          (e = Pe.get(c)) && xo(t, e),
          (r = n.createElement("script")),
          le(r),
          xe(r, "link", t),
          n.head.appendChild(r)),
        (r = { type: "script", instance: r, count: 1, state: null }),
        a.set(c, r));
    }
  }
  function Jm(t, e, n, a) {
    var c = (c = ot.current) ? Nc(c) : null;
    if (!c) throw Error(s(446));
    switch (t) {
      case "meta":
      case "title":
        return null;
      case "style":
        return typeof n.precedence == "string" && typeof n.href == "string"
          ? ((e = Ba(n.href)),
            (n = nl(c).hoistableStyles),
            (a = n.get(e)),
            a ||
              ((a = { type: "style", instance: null, count: 0, state: null }),
              n.set(e, a)),
            a)
          : { type: "void", instance: null, count: 0, state: null };
      case "link":
        if (
          n.rel === "stylesheet" &&
          typeof n.href == "string" &&
          typeof n.precedence == "string"
        ) {
          t = Ba(n.href);
          var r = nl(c).hoistableStyles,
            h = r.get(t);
          if (
            (h ||
              ((c = c.ownerDocument || c),
              (h = {
                type: "stylesheet",
                instance: null,
                count: 0,
                state: { loading: 0, preload: null },
              }),
              r.set(t, h),
              (r = c.querySelector(Vu(t))) &&
                !r._p &&
                ((h.instance = r), (h.state.loading = 5)),
              Pe.has(t) ||
                ((n = {
                  rel: "preload",
                  as: "style",
                  href: n.href,
                  crossOrigin: n.crossOrigin,
                  integrity: n.integrity,
                  media: n.media,
                  hrefLang: n.hrefLang,
                  referrerPolicy: n.referrerPolicy,
                }),
                Pe.set(t, n),
                r || Qv(c, t, n, h.state))),
            e && a === null)
          )
            throw Error(s(528, ""));
          return h;
        }
        if (e && a !== null) throw Error(s(529, ""));
        return null;
      case "script":
        return (
          (e = n.async),
          (n = n.src),
          typeof n == "string" &&
          e &&
          typeof e != "function" &&
          typeof e != "symbol"
            ? ((e = Ya(n)),
              (n = nl(c).hoistableScripts),
              (a = n.get(e)),
              a ||
                ((a = {
                  type: "script",
                  instance: null,
                  count: 0,
                  state: null,
                }),
                n.set(e, a)),
              a)
            : { type: "void", instance: null, count: 0, state: null }
        );
      default:
        throw Error(s(444, t));
    }
  }
  function Ba(t) {
    return 'href="' + Qe(t) + '"';
  }
  function Vu(t) {
    return 'link[rel="stylesheet"][' + t + "]";
  }
  function Wm(t) {
    return x({}, t, { "data-precedence": t.precedence, precedence: null });
  }
  function Qv(t, e, n, a) {
    t.querySelector('link[rel="preload"][as="style"][' + e + "]")
      ? (a.loading = 1)
      : ((e = t.createElement("link")),
        (a.preload = e),
        e.addEventListener("load", function () {
          return (a.loading |= 1);
        }),
        e.addEventListener("error", function () {
          return (a.loading |= 2);
        }),
        xe(e, "link", n),
        le(e),
        t.head.appendChild(e));
  }
  function Ya(t) {
    return '[src="' + Qe(t) + '"]';
  }
  function Lu(t) {
    return "script[async]" + t;
  }
  function Fm(t, e, n) {
    if ((e.count++, e.instance === null))
      switch (e.type) {
        case "style":
          var a = t.querySelector('style[data-href~="' + Qe(n.href) + '"]');
          if (a) return ((e.instance = a), le(a), a);
          var c = x({}, n, {
            "data-href": n.href,
            "data-precedence": n.precedence,
            href: null,
            precedence: null,
          });
          return (
            (a = (t.ownerDocument || t).createElement("style")),
            le(a),
            xe(a, "style", c),
            Ac(a, n.precedence, t),
            (e.instance = a)
          );
        case "stylesheet":
          c = Ba(n.href);
          var r = t.querySelector(Vu(c));
          if (r) return ((e.state.loading |= 4), (e.instance = r), le(r), r);
          ((a = Wm(n)),
            (c = Pe.get(c)) && vo(a, c),
            (r = (t.ownerDocument || t).createElement("link")),
            le(r));
          var h = r;
          return (
            (h._p = new Promise(function (v, T) {
              ((h.onload = v), (h.onerror = T));
            })),
            xe(r, "link", a),
            (e.state.loading |= 4),
            Ac(r, n.precedence, t),
            (e.instance = r)
          );
        case "script":
          return (
            (r = Ya(n.src)),
            (c = t.querySelector(Lu(r)))
              ? ((e.instance = c), le(c), c)
              : ((a = n),
                (c = Pe.get(r)) && ((a = x({}, n)), xo(a, c)),
                (t = t.ownerDocument || t),
                (c = t.createElement("script")),
                le(c),
                xe(c, "link", a),
                t.head.appendChild(c),
                (e.instance = c))
          );
        case "void":
          return null;
        default:
          throw Error(s(443, e.type));
      }
    else
      e.type === "stylesheet" &&
        (e.state.loading & 4) === 0 &&
        ((a = e.instance), (e.state.loading |= 4), Ac(a, n.precedence, t));
    return e.instance;
  }
  function Ac(t, e, n) {
    for (
      var a = n.querySelectorAll(
          'link[rel="stylesheet"][data-precedence],style[data-precedence]',
        ),
        c = a.length ? a[a.length - 1] : null,
        r = c,
        h = 0;
      h < a.length;
      h++
    ) {
      var v = a[h];
      if (v.dataset.precedence === e) r = v;
      else if (r !== c) break;
    }
    r
      ? r.parentNode.insertBefore(t, r.nextSibling)
      : ((e = n.nodeType === 9 ? n.head : n), e.insertBefore(t, e.firstChild));
  }
  function vo(t, e) {
    (t.crossOrigin == null && (t.crossOrigin = e.crossOrigin),
      t.referrerPolicy == null && (t.referrerPolicy = e.referrerPolicy),
      t.title == null && (t.title = e.title));
  }
  function xo(t, e) {
    (t.crossOrigin == null && (t.crossOrigin = e.crossOrigin),
      t.referrerPolicy == null && (t.referrerPolicy = e.referrerPolicy),
      t.integrity == null && (t.integrity = e.integrity));
  }
  var Tc = null;
  function Im(t, e, n) {
    if (Tc === null) {
      var a = new Map(),
        c = (Tc = new Map());
      c.set(n, a);
    } else ((c = Tc), (a = c.get(n)), a || ((a = new Map()), c.set(n, a)));
    if (a.has(t)) return a;
    for (
      a.set(t, null), n = n.getElementsByTagName(t), c = 0;
      c < n.length;
      c++
    ) {
      var r = n[c];
      if (
        !(
          r[jl] ||
          r[re] ||
          (t === "link" && r.getAttribute("rel") === "stylesheet")
        ) &&
        r.namespaceURI !== "http://www.w3.org/2000/svg"
      ) {
        var h = r.getAttribute(e) || "";
        h = t + h;
        var v = a.get(h);
        v ? v.push(r) : a.set(h, [r]);
      }
    }
    return a;
  }
  function Pm(t, e, n) {
    ((t = t.ownerDocument || t),
      t.head.insertBefore(
        n,
        e === "title" ? t.querySelector("head > title") : null,
      ));
  }
  function kv(t, e, n) {
    if (n === 1 || e.itemProp != null) return !1;
    switch (t) {
      case "meta":
      case "title":
        return !0;
      case "style":
        if (
          typeof e.precedence != "string" ||
          typeof e.href != "string" ||
          e.href === ""
        )
          break;
        return !0;
      case "link":
        if (
          typeof e.rel != "string" ||
          typeof e.href != "string" ||
          e.href === "" ||
          e.onLoad ||
          e.onError
        )
          break;
        switch (e.rel) {
          case "stylesheet":
            return (
              (t = e.disabled),
              typeof e.precedence == "string" && t == null
            );
          default:
            return !0;
        }
      case "script":
        if (
          e.async &&
          typeof e.async != "function" &&
          typeof e.async != "symbol" &&
          !e.onLoad &&
          !e.onError &&
          e.src &&
          typeof e.src == "string"
        )
          return !0;
    }
    return !1;
  }
  function t0(t) {
    return !(t.type === "stylesheet" && (t.state.loading & 3) === 0);
  }
  function Kv(t, e, n, a) {
    if (
      n.type === "stylesheet" &&
      (typeof a.media != "string" || matchMedia(a.media).matches !== !1) &&
      (n.state.loading & 4) === 0
    ) {
      if (n.instance === null) {
        var c = Ba(a.href),
          r = e.querySelector(Vu(c));
        if (r) {
          ((e = r._p),
            e !== null &&
              typeof e == "object" &&
              typeof e.then == "function" &&
              (t.count++, (t = zc.bind(t)), e.then(t, t)),
            (n.state.loading |= 4),
            (n.instance = r),
            le(r));
          return;
        }
        ((r = e.ownerDocument || e),
          (a = Wm(a)),
          (c = Pe.get(c)) && vo(a, c),
          (r = r.createElement("link")),
          le(r));
        var h = r;
        ((h._p = new Promise(function (v, T) {
          ((h.onload = v), (h.onerror = T));
        })),
          xe(r, "link", a),
          (n.instance = r));
      }
      (t.stylesheets === null && (t.stylesheets = new Map()),
        t.stylesheets.set(n, e),
        (e = n.state.preload) &&
          (n.state.loading & 3) === 0 &&
          (t.count++,
          (n = zc.bind(t)),
          e.addEventListener("load", n),
          e.addEventListener("error", n)));
    }
  }
  var bo = 0;
  function $v(t, e) {
    return (
      t.stylesheets && t.count === 0 && Cc(t, t.stylesheets),
      0 < t.count || 0 < t.imgCount
        ? function (n) {
            var a = setTimeout(function () {
              if ((t.stylesheets && Cc(t, t.stylesheets), t.unsuspend)) {
                var r = t.unsuspend;
                ((t.unsuspend = null), r());
              }
            }, 6e4 + e);
            0 < t.imgBytes && bo === 0 && (bo = 62500 * zv());
            var c = setTimeout(
              function () {
                if (
                  ((t.waitingForImages = !1),
                  t.count === 0 &&
                    (t.stylesheets && Cc(t, t.stylesheets), t.unsuspend))
                ) {
                  var r = t.unsuspend;
                  ((t.unsuspend = null), r());
                }
              },
              (t.imgBytes > bo ? 50 : 800) + e,
            );
            return (
              (t.unsuspend = n),
              function () {
                ((t.unsuspend = null), clearTimeout(a), clearTimeout(c));
              }
            );
          }
        : null
    );
  }
  function zc() {
    if (
      (this.count--,
      this.count === 0 && (this.imgCount === 0 || !this.waitingForImages))
    ) {
      if (this.stylesheets) Cc(this, this.stylesheets);
      else if (this.unsuspend) {
        var t = this.unsuspend;
        ((this.unsuspend = null), t());
      }
    }
  }
  var Mc = null;
  function Cc(t, e) {
    ((t.stylesheets = null),
      t.unsuspend !== null &&
        (t.count++,
        (Mc = new Map()),
        e.forEach(Jv, t),
        (Mc = null),
        zc.call(t)));
  }
  function Jv(t, e) {
    if (!(e.state.loading & 4)) {
      var n = Mc.get(t);
      if (n) var a = n.get(null);
      else {
        ((n = new Map()), Mc.set(t, n));
        for (
          var c = t.querySelectorAll(
              "link[data-precedence],style[data-precedence]",
            ),
            r = 0;
          r < c.length;
          r++
        ) {
          var h = c[r];
          (h.nodeName === "LINK" || h.getAttribute("media") !== "not all") &&
            (n.set(h.dataset.precedence, h), (a = h));
        }
        a && n.set(null, a);
      }
      ((c = e.instance),
        (h = c.getAttribute("data-precedence")),
        (r = n.get(h) || a),
        r === a && n.set(null, c),
        n.set(h, c),
        this.count++,
        (a = zc.bind(this)),
        c.addEventListener("load", a),
        c.addEventListener("error", a),
        r
          ? r.parentNode.insertBefore(c, r.nextSibling)
          : ((t = t.nodeType === 9 ? t.head : t),
            t.insertBefore(c, t.firstChild)),
        (e.state.loading |= 4));
    }
  }
  var Gu = {
    $$typeof: G,
    Provider: null,
    Consumer: null,
    _currentValue: Z,
    _currentValue2: Z,
    _threadCount: 0,
  };
  function Wv(t, e, n, a, c, r, h, v, T) {
    ((this.tag = 1),
      (this.containerInfo = t),
      (this.pingCache = this.current = this.pendingChildren = null),
      (this.timeoutHandle = -1),
      (this.callbackNode =
        this.next =
        this.pendingContext =
        this.context =
        this.cancelPendingCommit =
          null),
      (this.callbackPriority = 0),
      (this.expirationTimes = tu(-1)),
      (this.entangledLanes =
        this.shellSuspendCounter =
        this.errorRecoveryDisabledLanes =
        this.expiredLanes =
        this.warmLanes =
        this.pingedLanes =
        this.suspendedLanes =
        this.pendingLanes =
          0),
      (this.entanglements = tu(0)),
      (this.hiddenUpdates = tu(null)),
      (this.identifierPrefix = a),
      (this.onUncaughtError = c),
      (this.onCaughtError = r),
      (this.onRecoverableError = h),
      (this.pooledCache = null),
      (this.pooledCacheLanes = 0),
      (this.formState = T),
      (this.incompleteTransitions = new Map()));
  }
  function e0(t, e, n, a, c, r, h, v, T, X, P, lt) {
    return (
      (t = new Wv(t, e, n, h, T, X, P, lt, v)),
      (e = 1),
      r === !0 && (e |= 24),
      (r = Ue(3, null, null, e)),
      (t.current = r),
      (r.stateNode = t),
      (e = Is()),
      e.refCount++,
      (t.pooledCache = e),
      e.refCount++,
      (r.memoizedState = { element: a, isDehydrated: n, cache: e }),
      nr(r),
      t
    );
  }
  function n0(t) {
    return t ? ((t = ga), t) : ga;
  }
  function l0(t, e, n, a, c, r) {
    ((c = n0(c)),
      a.context === null ? (a.context = c) : (a.pendingContext = c),
      (a = rl(e)),
      (a.payload = { element: n }),
      (r = r === void 0 ? null : r),
      r !== null && (a.callback = r),
      (n = ol(t, a, e)),
      n !== null && (Ce(n, t, e), Su(n, t, e)));
  }
  function a0(t, e) {
    if (((t = t.memoizedState), t !== null && t.dehydrated !== null)) {
      var n = t.retryLane;
      t.retryLane = n !== 0 && n < e ? n : e;
    }
  }
  function So(t, e) {
    (a0(t, e), (t = t.alternate) && a0(t, e));
  }
  function u0(t) {
    if (t.tag === 13 || t.tag === 31) {
      var e = Xl(t, 67108864);
      (e !== null && Ce(e, t, 67108864), So(t, 67108864));
    }
  }
  function i0(t) {
    if (t.tag === 13 || t.tag === 31) {
      var e = Ve();
      e = eu(e);
      var n = Xl(t, e);
      (n !== null && Ce(n, t, e), So(t, e));
    }
  }
  var Dc = !0;
  function Fv(t, e, n, a) {
    var c = S.T;
    S.T = null;
    var r = R.p;
    try {
      ((R.p = 2), Eo(t, e, n, a));
    } finally {
      ((R.p = r), (S.T = c));
    }
  }
  function Iv(t, e, n, a) {
    var c = S.T;
    S.T = null;
    var r = R.p;
    try {
      ((R.p = 8), Eo(t, e, n, a));
    } finally {
      ((R.p = r), (S.T = c));
    }
  }
  function Eo(t, e, n, a) {
    if (Dc) {
      var c = _o(a);
      if (c === null) (co(t, e, a, Oc, n), s0(t, a));
      else if (t1(c, t, e, n, a)) a.stopPropagation();
      else if ((s0(t, a), e & 4 && -1 < Pv.indexOf(t))) {
        for (; c !== null; ) {
          var r = En(c);
          if (r !== null)
            switch (r.tag) {
              case 3:
                if (((r = r.stateNode), r.current.memoizedState.isDehydrated)) {
                  var h = _e(r.pendingLanes);
                  if (h !== 0) {
                    var v = r;
                    for (v.pendingLanes |= 2, v.entangledLanes |= 2; h; ) {
                      var T = 1 << (31 - $t(h));
                      ((v.entanglements[1] |= T), (h &= ~T));
                    }
                    (vn(r), (Ht & 6) === 0 && ((mc = Kt() + 500), Bu(0)));
                  }
                }
                break;
              case 31:
              case 13:
                ((v = Xl(r, 2)), v !== null && Ce(v, r, 2), yc(), So(r, 2));
            }
          if (((r = _o(a)), r === null && co(t, e, a, Oc, n), r === c)) break;
          c = r;
        }
        c !== null && a.stopPropagation();
      } else co(t, e, a, null, n);
    }
  }
  function _o(t) {
    return ((t = ws(t)), wo(t));
  }
  var Oc = null;
  function wo(t) {
    if (((Oc = null), (t = el(t)), t !== null)) {
      var e = f(t);
      if (e === null) t = null;
      else {
        var n = e.tag;
        if (n === 13) {
          if (((t = d(e)), t !== null)) return t;
          t = null;
        } else if (n === 31) {
          if (((t = g(e)), t !== null)) return t;
          t = null;
        } else if (n === 3) {
          if (e.stateNode.current.memoizedState.isDehydrated)
            return e.tag === 3 ? e.stateNode.containerInfo : null;
          t = null;
        } else e !== t && (t = null);
      }
    }
    return ((Oc = t), null);
  }
  function c0(t) {
    switch (t) {
      case "beforetoggle":
      case "cancel":
      case "click":
      case "close":
      case "contextmenu":
      case "copy":
      case "cut":
      case "auxclick":
      case "dblclick":
      case "dragend":
      case "dragstart":
      case "drop":
      case "focusin":
      case "focusout":
      case "input":
      case "invalid":
      case "keydown":
      case "keypress":
      case "keyup":
      case "mousedown":
      case "mouseup":
      case "paste":
      case "pause":
      case "play":
      case "pointercancel":
      case "pointerdown":
      case "pointerup":
      case "ratechange":
      case "reset":
      case "resize":
      case "seeked":
      case "submit":
      case "toggle":
      case "touchcancel":
      case "touchend":
      case "touchstart":
      case "volumechange":
      case "change":
      case "selectionchange":
      case "textInput":
      case "compositionstart":
      case "compositionend":
      case "compositionupdate":
      case "beforeblur":
      case "afterblur":
      case "beforeinput":
      case "blur":
      case "fullscreenchange":
      case "focus":
      case "hashchange":
      case "popstate":
      case "select":
      case "selectstart":
        return 2;
      case "drag":
      case "dragenter":
      case "dragexit":
      case "dragleave":
      case "dragover":
      case "mousemove":
      case "mouseout":
      case "mouseover":
      case "pointermove":
      case "pointerout":
      case "pointerover":
      case "scroll":
      case "touchmove":
      case "wheel":
      case "mouseenter":
      case "mouseleave":
      case "pointerenter":
      case "pointerleave":
        return 8;
      case "message":
        switch (Wn()) {
          case fn:
            return 2;
          case dn:
            return 8;
          case nn:
          case Fn:
            return 32;
          case In:
            return 268435456;
          default:
            return 32;
        }
      default:
        return 32;
    }
  }
  var No = !1,
    Sl = null,
    El = null,
    _l = null,
    Zu = new Map(),
    Qu = new Map(),
    wl = [],
    Pv =
      "mousedown mouseup touchcancel touchend touchstart auxclick dblclick pointercancel pointerdown pointerup dragend dragstart drop compositionend compositionstart keydown keypress keyup input textInput copy cut paste click change contextmenu reset".split(
        " ",
      );
  function s0(t, e) {
    switch (t) {
      case "focusin":
      case "focusout":
        Sl = null;
        break;
      case "dragenter":
      case "dragleave":
        El = null;
        break;
      case "mouseover":
      case "mouseout":
        _l = null;
        break;
      case "pointerover":
      case "pointerout":
        Zu.delete(e.pointerId);
        break;
      case "gotpointercapture":
      case "lostpointercapture":
        Qu.delete(e.pointerId);
    }
  }
  function ku(t, e, n, a, c, r) {
    return t === null || t.nativeEvent !== r
      ? ((t = {
          blockedOn: e,
          domEventName: n,
          eventSystemFlags: a,
          nativeEvent: r,
          targetContainers: [c],
        }),
        e !== null && ((e = En(e)), e !== null && u0(e)),
        t)
      : ((t.eventSystemFlags |= a),
        (e = t.targetContainers),
        c !== null && e.indexOf(c) === -1 && e.push(c),
        t);
  }
  function t1(t, e, n, a, c) {
    switch (e) {
      case "focusin":
        return ((Sl = ku(Sl, t, e, n, a, c)), !0);
      case "dragenter":
        return ((El = ku(El, t, e, n, a, c)), !0);
      case "mouseover":
        return ((_l = ku(_l, t, e, n, a, c)), !0);
      case "pointerover":
        var r = c.pointerId;
        return (Zu.set(r, ku(Zu.get(r) || null, t, e, n, a, c)), !0);
      case "gotpointercapture":
        return (
          (r = c.pointerId),
          Qu.set(r, ku(Qu.get(r) || null, t, e, n, a, c)),
          !0
        );
    }
    return !1;
  }
  function r0(t) {
    var e = el(t.target);
    if (e !== null) {
      var n = f(e);
      if (n !== null) {
        if (((e = n.tag), e === 13)) {
          if (((e = d(n)), e !== null)) {
            ((t.blockedOn = e),
              bi(t.priority, function () {
                i0(n);
              }));
            return;
          }
        } else if (e === 31) {
          if (((e = g(n)), e !== null)) {
            ((t.blockedOn = e),
              bi(t.priority, function () {
                i0(n);
              }));
            return;
          }
        } else if (e === 3 && n.stateNode.current.memoizedState.isDehydrated) {
          t.blockedOn = n.tag === 3 ? n.stateNode.containerInfo : null;
          return;
        }
      }
    }
    t.blockedOn = null;
  }
  function Rc(t) {
    if (t.blockedOn !== null) return !1;
    for (var e = t.targetContainers; 0 < e.length; ) {
      var n = _o(t.nativeEvent);
      if (n === null) {
        n = t.nativeEvent;
        var a = new n.constructor(n.type, n);
        ((_s = a), n.target.dispatchEvent(a), (_s = null));
      } else return ((e = En(n)), e !== null && u0(e), (t.blockedOn = n), !1);
      e.shift();
    }
    return !0;
  }
  function o0(t, e, n) {
    Rc(t) && n.delete(e);
  }
  function e1() {
    ((No = !1),
      Sl !== null && Rc(Sl) && (Sl = null),
      El !== null && Rc(El) && (El = null),
      _l !== null && Rc(_l) && (_l = null),
      Zu.forEach(o0),
      Qu.forEach(o0));
  }
  function Hc(t, e) {
    t.blockedOn === e &&
      ((t.blockedOn = null),
      No ||
        ((No = !0),
        l.unstable_scheduleCallback(l.unstable_NormalPriority, e1)));
  }
  var jc = null;
  function f0(t) {
    jc !== t &&
      ((jc = t),
      l.unstable_scheduleCallback(l.unstable_NormalPriority, function () {
        jc === t && (jc = null);
        for (var e = 0; e < t.length; e += 3) {
          var n = t[e],
            a = t[e + 1],
            c = t[e + 2];
          if (typeof a != "function") {
            if (wo(a || n) === null) continue;
            break;
          }
          var r = En(n);
          r !== null &&
            (t.splice(e, 3),
            (e -= 3),
            Er(r, { pending: !0, data: c, method: n.method, action: a }, a, c));
        }
      }));
  }
  function qa(t) {
    function e(T) {
      return Hc(T, t);
    }
    (Sl !== null && Hc(Sl, t),
      El !== null && Hc(El, t),
      _l !== null && Hc(_l, t),
      Zu.forEach(e),
      Qu.forEach(e));
    for (var n = 0; n < wl.length; n++) {
      var a = wl[n];
      a.blockedOn === t && (a.blockedOn = null);
    }
    for (; 0 < wl.length && ((n = wl[0]), n.blockedOn === null); )
      (r0(n), n.blockedOn === null && wl.shift());
    if (((n = (t.ownerDocument || t).$$reactFormReplay), n != null))
      for (a = 0; a < n.length; a += 3) {
        var c = n[a],
          r = n[a + 1],
          h = c[ge] || null;
        if (typeof r == "function") h || f0(n);
        else if (h) {
          var v = null;
          if (r && r.hasAttribute("formAction")) {
            if (((c = r), (h = r[ge] || null))) v = h.formAction;
            else if (wo(c) !== null) continue;
          } else v = h.action;
          (typeof v == "function" ? (n[a + 1] = v) : (n.splice(a, 3), (a -= 3)),
            f0(n));
        }
      }
  }
  function d0() {
    function t(r) {
      r.canIntercept &&
        r.info === "react-transition" &&
        r.intercept({
          handler: function () {
            return new Promise(function (h) {
              return (c = h);
            });
          },
          focusReset: "manual",
          scroll: "manual",
        });
    }
    function e() {
      (c !== null && (c(), (c = null)), a || setTimeout(n, 20));
    }
    function n() {
      if (!a && !navigation.transition) {
        var r = navigation.currentEntry;
        r &&
          r.url != null &&
          navigation.navigate(r.url, {
            state: r.getState(),
            info: "react-transition",
            history: "replace",
          });
      }
    }
    if (typeof navigation == "object") {
      var a = !1,
        c = null;
      return (
        navigation.addEventListener("navigate", t),
        navigation.addEventListener("navigatesuccess", e),
        navigation.addEventListener("navigateerror", e),
        setTimeout(n, 100),
        function () {
          ((a = !0),
            navigation.removeEventListener("navigate", t),
            navigation.removeEventListener("navigatesuccess", e),
            navigation.removeEventListener("navigateerror", e),
            c !== null && (c(), (c = null)));
        }
      );
    }
  }
  function Ao(t) {
    this._internalRoot = t;
  }
  ((Uc.prototype.render = Ao.prototype.render =
    function (t) {
      var e = this._internalRoot;
      if (e === null) throw Error(s(409));
      var n = e.current,
        a = Ve();
      l0(n, a, t, e, null, null);
    }),
    (Uc.prototype.unmount = Ao.prototype.unmount =
      function () {
        var t = this._internalRoot;
        if (t !== null) {
          this._internalRoot = null;
          var e = t.containerInfo;
          (l0(t.current, 2, null, t, null, null), yc(), (e[tl] = null));
        }
      }));
  function Uc(t) {
    this._internalRoot = t;
  }
  Uc.prototype.unstable_scheduleHydration = function (t) {
    if (t) {
      var e = xi();
      t = { blockedOn: null, target: t, priority: e };
      for (var n = 0; n < wl.length && e !== 0 && e < wl[n].priority; n++);
      (wl.splice(n, 0, t), n === 0 && r0(t));
    }
  };
  var h0 = u.version;
  if (h0 !== "19.2.4") throw Error(s(527, h0, "19.2.4"));
  R.findDOMNode = function (t) {
    var e = t._reactInternals;
    if (e === void 0)
      throw typeof t.render == "function"
        ? Error(s(188))
        : ((t = Object.keys(t).join(",")), Error(s(268, t)));
    return (
      (t = m(e)),
      (t = t !== null ? p(t) : null),
      (t = t === null ? null : t.stateNode),
      t
    );
  };
  var n1 = {
    bundleType: 0,
    version: "19.2.4",
    rendererPackageName: "react-dom",
    currentDispatcherRef: S,
    reconcilerVersion: "19.2.4",
  };
  if (typeof __REACT_DEVTOOLS_GLOBAL_HOOK__ < "u") {
    var Bc = __REACT_DEVTOOLS_GLOBAL_HOOK__;
    if (!Bc.isDisabled && Bc.supportsFiber)
      try {
        ((He = Bc.inject(n1)), (Pt = Bc));
      } catch {}
  }
  return (
    ($u.createRoot = function (t, e) {
      if (!o(t)) throw Error(s(299));
      var n = !1,
        a = "",
        c = bh,
        r = Sh,
        h = Eh;
      return (
        e != null &&
          (e.unstable_strictMode === !0 && (n = !0),
          e.identifierPrefix !== void 0 && (a = e.identifierPrefix),
          e.onUncaughtError !== void 0 && (c = e.onUncaughtError),
          e.onCaughtError !== void 0 && (r = e.onCaughtError),
          e.onRecoverableError !== void 0 && (h = e.onRecoverableError)),
        (e = e0(t, 1, !1, null, null, n, a, null, c, r, h, d0)),
        (t[tl] = e.current),
        io(t),
        new Ao(e)
      );
    }),
    ($u.hydrateRoot = function (t, e, n) {
      if (!o(t)) throw Error(s(299));
      var a = !1,
        c = "",
        r = bh,
        h = Sh,
        v = Eh,
        T = null;
      return (
        n != null &&
          (n.unstable_strictMode === !0 && (a = !0),
          n.identifierPrefix !== void 0 && (c = n.identifierPrefix),
          n.onUncaughtError !== void 0 && (r = n.onUncaughtError),
          n.onCaughtError !== void 0 && (h = n.onCaughtError),
          n.onRecoverableError !== void 0 && (v = n.onRecoverableError),
          n.formState !== void 0 && (T = n.formState)),
        (e = e0(t, 1, !0, e, n ?? null, a, c, T, r, h, v, d0)),
        (e.context = n0(null)),
        (n = e.current),
        (a = Ve()),
        (a = eu(a)),
        (c = rl(a)),
        (c.callback = null),
        ol(n, c, a),
        (n = a),
        (e.current.lanes = n),
        Hl(e, n),
        vn(e),
        (t[tl] = e.current),
        io(t),
        new Uc(e)
      );
    }),
    ($u.version = "19.2.4"),
    $u
  );
}
var _0;
function f1() {
  if (_0) return Mo.exports;
  _0 = 1;
  function l() {
    if (
      !(
        typeof __REACT_DEVTOOLS_GLOBAL_HOOK__ > "u" ||
        typeof __REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE != "function"
      )
    )
      try {
        __REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE(l);
      } catch (u) {
        console.error(u);
      }
  }
  return (l(), (Mo.exports = o1()), Mo.exports);
}
var d1 = f1();
const h1 = of(d1);
function be(l) {
  if (typeof l == "string" || typeof l == "number") return "" + l;
  let u = "";
  if (Array.isArray(l))
    for (let i = 0, s; i < l.length; i++)
      (s = be(l[i])) !== "" && (u += (u && " ") + s);
  else for (let i in l) l[i] && (u += (u && " ") + i);
  return u;
}
var Ro = { exports: {} },
  Ho = {},
  jo = { exports: {} },
  Uo = {};
/**
 * @license React
 * use-sync-external-store-shim.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */ var w0;
function m1() {
  if (w0) return Uo;
  w0 = 1;
  var l = fi();
  function u(x, _) {
    return (x === _ && (x !== 0 || 1 / x === 1 / _)) || (x !== x && _ !== _);
  }
  var i = typeof Object.is == "function" ? Object.is : u,
    s = l.useState,
    o = l.useEffect,
    f = l.useLayoutEffect,
    d = l.useDebugValue;
  function g(x, _) {
    var E = _(),
      D = s({ inst: { value: E, getSnapshot: _ } }),
      z = D[0].inst,
      B = D[1];
    return (
      f(
        function () {
          ((z.value = E), (z.getSnapshot = _), y(z) && B({ inst: z }));
        },
        [x, E, _],
      ),
      o(
        function () {
          return (
            y(z) && B({ inst: z }),
            x(function () {
              y(z) && B({ inst: z });
            })
          );
        },
        [x],
      ),
      d(E),
      E
    );
  }
  function y(x) {
    var _ = x.getSnapshot;
    x = x.value;
    try {
      var E = _();
      return !i(x, E);
    } catch {
      return !0;
    }
  }
  function m(x, _) {
    return _();
  }
  var p =
    typeof window > "u" ||
    typeof window.document > "u" ||
    typeof window.document.createElement > "u"
      ? m
      : g;
  return (
    (Uo.useSyncExternalStore =
      l.useSyncExternalStore !== void 0 ? l.useSyncExternalStore : p),
    Uo
  );
}
var N0;
function g1() {
  return (N0 || ((N0 = 1), (jo.exports = m1())), jo.exports);
}
/**
 * @license React
 * use-sync-external-store-shim/with-selector.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */ var A0;
function y1() {
  if (A0) return Ho;
  A0 = 1;
  var l = fi(),
    u = g1();
  function i(m, p) {
    return (m === p && (m !== 0 || 1 / m === 1 / p)) || (m !== m && p !== p);
  }
  var s = typeof Object.is == "function" ? Object.is : i,
    o = u.useSyncExternalStore,
    f = l.useRef,
    d = l.useEffect,
    g = l.useMemo,
    y = l.useDebugValue;
  return (
    (Ho.useSyncExternalStoreWithSelector = function (m, p, x, _, E) {
      var D = f(null);
      if (D.current === null) {
        var z = { hasValue: !1, value: null };
        D.current = z;
      } else z = D.current;
      D = g(
        function () {
          function N(I) {
            if (!Y) {
              if (((Y = !0), (G = I), (I = _(I)), E !== void 0 && z.hasValue)) {
                var Q = z.value;
                if (E(Q, I)) return (j = Q);
              }
              return (j = I);
            }
            if (((Q = j), s(G, I))) return Q;
            var ut = _(I);
            return E !== void 0 && E(Q, ut)
              ? ((G = I), Q)
              : ((G = I), (j = ut));
          }
          var Y = !1,
            G,
            j,
            W = x === void 0 ? null : x;
          return [
            function () {
              return N(p());
            },
            W === null
              ? void 0
              : function () {
                  return N(W());
                },
          ];
        },
        [p, x, _, E],
      );
      var B = o(m, D[0], D[1]);
      return (
        d(
          function () {
            ((z.hasValue = !0), (z.value = B));
          },
          [B],
        ),
        y(B),
        B
      );
    }),
    Ho
  );
}
var T0;
function p1() {
  return (T0 || ((T0 = 1), (Ro.exports = y1())), Ro.exports);
}
var v1 = p1();
const Sg = of(v1),
  x1 = {},
  z0 = (l) => {
    let u;
    const i = new Set(),
      s = (p, x) => {
        const _ = typeof p == "function" ? p(u) : p;
        if (!Object.is(_, u)) {
          const E = u;
          ((u =
            (x ?? (typeof _ != "object" || _ === null))
              ? _
              : Object.assign({}, u, _)),
            i.forEach((D) => D(u, E)));
        }
      },
      o = () => u,
      y = {
        setState: s,
        getState: o,
        getInitialState: () => m,
        subscribe: (p) => (i.add(p), () => i.delete(p)),
        destroy: () => {
          ((x1 ? "production" : void 0) !== "production" &&
            console.warn(
              "[DEPRECATED] The `destroy` method will be unsupported in a future version. Instead use unsubscribe function returned by subscribe. Everything will be garbage-collected if store is garbage-collected.",
            ),
            i.clear());
        },
      },
      m = (u = l(s, o, y));
    return y;
  },
  Eg = (l) => (l ? z0(l) : z0),
  { useDebugValue: b1 } = J,
  { useSyncExternalStoreWithSelector: S1 } = Sg,
  E1 = (l) => l;
function _g(l, u = E1, i) {
  const s = S1(
    l.subscribe,
    l.getState,
    l.getServerState || l.getInitialState,
    u,
    i,
  );
  return (b1(s), s);
}
const M0 = (l, u) => {
    const i = Eg(l),
      s = (o, f = u) => _g(i, o, f);
    return (Object.assign(s, i), s);
  },
  _1 = (l, u) => (l ? M0(l, u) : M0);
function me(l, u) {
  if (Object.is(l, u)) return !0;
  if (typeof l != "object" || l === null || typeof u != "object" || u === null)
    return !1;
  if (l instanceof Map && u instanceof Map) {
    if (l.size !== u.size) return !1;
    for (const [s, o] of l) if (!Object.is(o, u.get(s))) return !1;
    return !0;
  }
  if (l instanceof Set && u instanceof Set) {
    if (l.size !== u.size) return !1;
    for (const s of l) if (!u.has(s)) return !1;
    return !0;
  }
  const i = Object.keys(l);
  if (i.length !== Object.keys(u).length) return !1;
  for (const s of i)
    if (!Object.prototype.hasOwnProperty.call(u, s) || !Object.is(l[s], u[s]))
      return !1;
  return !0;
}
var w1 = { value: () => {} };
function is() {
  for (var l = 0, u = arguments.length, i = {}, s; l < u; ++l) {
    if (!(s = arguments[l] + "") || s in i || /[\s.]/.test(s))
      throw new Error("illegal type: " + s);
    i[s] = [];
  }
  return new Kc(i);
}
function Kc(l) {
  this._ = l;
}
function N1(l, u) {
  return l
    .trim()
    .split(/^|\s+/)
    .map(function (i) {
      var s = "",
        o = i.indexOf(".");
      if (
        (o >= 0 && ((s = i.slice(o + 1)), (i = i.slice(0, o))),
        i && !u.hasOwnProperty(i))
      )
        throw new Error("unknown type: " + i);
      return { type: i, name: s };
    });
}
Kc.prototype = is.prototype = {
  constructor: Kc,
  on: function (l, u) {
    var i = this._,
      s = N1(l + "", i),
      o,
      f = -1,
      d = s.length;
    if (arguments.length < 2) {
      for (; ++f < d; )
        if ((o = (l = s[f]).type) && (o = A1(i[o], l.name))) return o;
      return;
    }
    if (u != null && typeof u != "function")
      throw new Error("invalid callback: " + u);
    for (; ++f < d; )
      if ((o = (l = s[f]).type)) i[o] = C0(i[o], l.name, u);
      else if (u == null) for (o in i) i[o] = C0(i[o], l.name, null);
    return this;
  },
  copy: function () {
    var l = {},
      u = this._;
    for (var i in u) l[i] = u[i].slice();
    return new Kc(l);
  },
  call: function (l, u) {
    if ((o = arguments.length - 2) > 0)
      for (var i = new Array(o), s = 0, o, f; s < o; ++s)
        i[s] = arguments[s + 2];
    if (!this._.hasOwnProperty(l)) throw new Error("unknown type: " + l);
    for (f = this._[l], s = 0, o = f.length; s < o; ++s) f[s].value.apply(u, i);
  },
  apply: function (l, u, i) {
    if (!this._.hasOwnProperty(l)) throw new Error("unknown type: " + l);
    for (var s = this._[l], o = 0, f = s.length; o < f; ++o)
      s[o].value.apply(u, i);
  },
};
function A1(l, u) {
  for (var i = 0, s = l.length, o; i < s; ++i)
    if ((o = l[i]).name === u) return o.value;
}
function C0(l, u, i) {
  for (var s = 0, o = l.length; s < o; ++s)
    if (l[s].name === u) {
      ((l[s] = w1), (l = l.slice(0, s).concat(l.slice(s + 1))));
      break;
    }
  return (i != null && l.push({ name: u, value: i }), l);
}
var Ko = "http://www.w3.org/1999/xhtml";
const D0 = {
  svg: "http://www.w3.org/2000/svg",
  xhtml: Ko,
  xlink: "http://www.w3.org/1999/xlink",
  xml: "http://www.w3.org/XML/1998/namespace",
  xmlns: "http://www.w3.org/2000/xmlns/",
};
function cs(l) {
  var u = (l += ""),
    i = u.indexOf(":");
  return (
    i >= 0 && (u = l.slice(0, i)) !== "xmlns" && (l = l.slice(i + 1)),
    D0.hasOwnProperty(u) ? { space: D0[u], local: l } : l
  );
}
function T1(l) {
  return function () {
    var u = this.ownerDocument,
      i = this.namespaceURI;
    return i === Ko && u.documentElement.namespaceURI === Ko
      ? u.createElement(l)
      : u.createElementNS(i, l);
  };
}
function z1(l) {
  return function () {
    return this.ownerDocument.createElementNS(l.space, l.local);
  };
}
function wg(l) {
  var u = cs(l);
  return (u.local ? z1 : T1)(u);
}
function M1() {}
function ff(l) {
  return l == null
    ? M1
    : function () {
        return this.querySelector(l);
      };
}
function C1(l) {
  typeof l != "function" && (l = ff(l));
  for (var u = this._groups, i = u.length, s = new Array(i), o = 0; o < i; ++o)
    for (
      var f = u[o], d = f.length, g = (s[o] = new Array(d)), y, m, p = 0;
      p < d;
      ++p
    )
      (y = f[p]) &&
        (m = l.call(y, y.__data__, p, f)) &&
        ("__data__" in y && (m.__data__ = y.__data__), (g[p] = m));
  return new Le(s, this._parents);
}
function D1(l) {
  return l == null ? [] : Array.isArray(l) ? l : Array.from(l);
}
function O1() {
  return [];
}
function Ng(l) {
  return l == null
    ? O1
    : function () {
        return this.querySelectorAll(l);
      };
}
function R1(l) {
  return function () {
    return D1(l.apply(this, arguments));
  };
}
function H1(l) {
  typeof l == "function" ? (l = R1(l)) : (l = Ng(l));
  for (var u = this._groups, i = u.length, s = [], o = [], f = 0; f < i; ++f)
    for (var d = u[f], g = d.length, y, m = 0; m < g; ++m)
      (y = d[m]) && (s.push(l.call(y, y.__data__, m, d)), o.push(y));
  return new Le(s, o);
}
function Ag(l) {
  return function () {
    return this.matches(l);
  };
}
function Tg(l) {
  return function (u) {
    return u.matches(l);
  };
}
var j1 = Array.prototype.find;
function U1(l) {
  return function () {
    return j1.call(this.children, l);
  };
}
function B1() {
  return this.firstElementChild;
}
function Y1(l) {
  return this.select(l == null ? B1 : U1(typeof l == "function" ? l : Tg(l)));
}
var q1 = Array.prototype.filter;
function X1() {
  return Array.from(this.children);
}
function V1(l) {
  return function () {
    return q1.call(this.children, l);
  };
}
function L1(l) {
  return this.selectAll(
    l == null ? X1 : V1(typeof l == "function" ? l : Tg(l)),
  );
}
function G1(l) {
  typeof l != "function" && (l = Ag(l));
  for (var u = this._groups, i = u.length, s = new Array(i), o = 0; o < i; ++o)
    for (var f = u[o], d = f.length, g = (s[o] = []), y, m = 0; m < d; ++m)
      (y = f[m]) && l.call(y, y.__data__, m, f) && g.push(y);
  return new Le(s, this._parents);
}
function zg(l) {
  return new Array(l.length);
}
function Z1() {
  return new Le(this._enter || this._groups.map(zg), this._parents);
}
function Fc(l, u) {
  ((this.ownerDocument = l.ownerDocument),
    (this.namespaceURI = l.namespaceURI),
    (this._next = null),
    (this._parent = l),
    (this.__data__ = u));
}
Fc.prototype = {
  constructor: Fc,
  appendChild: function (l) {
    return this._parent.insertBefore(l, this._next);
  },
  insertBefore: function (l, u) {
    return this._parent.insertBefore(l, u);
  },
  querySelector: function (l) {
    return this._parent.querySelector(l);
  },
  querySelectorAll: function (l) {
    return this._parent.querySelectorAll(l);
  },
};
function Q1(l) {
  return function () {
    return l;
  };
}
function k1(l, u, i, s, o, f) {
  for (var d = 0, g, y = u.length, m = f.length; d < m; ++d)
    (g = u[d]) ? ((g.__data__ = f[d]), (s[d] = g)) : (i[d] = new Fc(l, f[d]));
  for (; d < y; ++d) (g = u[d]) && (o[d] = g);
}
function K1(l, u, i, s, o, f, d) {
  var g,
    y,
    m = new Map(),
    p = u.length,
    x = f.length,
    _ = new Array(p),
    E;
  for (g = 0; g < p; ++g)
    (y = u[g]) &&
      ((_[g] = E = d.call(y, y.__data__, g, u) + ""),
      m.has(E) ? (o[g] = y) : m.set(E, y));
  for (g = 0; g < x; ++g)
    ((E = d.call(l, f[g], g, f) + ""),
      (y = m.get(E))
        ? ((s[g] = y), (y.__data__ = f[g]), m.delete(E))
        : (i[g] = new Fc(l, f[g])));
  for (g = 0; g < p; ++g) (y = u[g]) && m.get(_[g]) === y && (o[g] = y);
}
function $1(l) {
  return l.__data__;
}
function J1(l, u) {
  if (!arguments.length) return Array.from(this, $1);
  var i = u ? K1 : k1,
    s = this._parents,
    o = this._groups;
  typeof l != "function" && (l = Q1(l));
  for (
    var f = o.length,
      d = new Array(f),
      g = new Array(f),
      y = new Array(f),
      m = 0;
    m < f;
    ++m
  ) {
    var p = s[m],
      x = o[m],
      _ = x.length,
      E = W1(l.call(p, p && p.__data__, m, s)),
      D = E.length,
      z = (g[m] = new Array(D)),
      B = (d[m] = new Array(D)),
      N = (y[m] = new Array(_));
    i(p, x, z, B, N, E, u);
    for (var Y = 0, G = 0, j, W; Y < D; ++Y)
      if ((j = z[Y])) {
        for (Y >= G && (G = Y + 1); !(W = B[G]) && ++G < D; );
        j._next = W || null;
      }
  }
  return ((d = new Le(d, s)), (d._enter = g), (d._exit = y), d);
}
function W1(l) {
  return typeof l == "object" && "length" in l ? l : Array.from(l);
}
function F1() {
  return new Le(this._exit || this._groups.map(zg), this._parents);
}
function I1(l, u, i) {
  var s = this.enter(),
    o = this,
    f = this.exit();
  return (
    typeof l == "function"
      ? ((s = l(s)), s && (s = s.selection()))
      : (s = s.append(l + "")),
    u != null && ((o = u(o)), o && (o = o.selection())),
    i == null ? f.remove() : i(f),
    s && o ? s.merge(o).order() : o
  );
}
function P1(l) {
  for (
    var u = l.selection ? l.selection() : l,
      i = this._groups,
      s = u._groups,
      o = i.length,
      f = s.length,
      d = Math.min(o, f),
      g = new Array(o),
      y = 0;
    y < d;
    ++y
  )
    for (
      var m = i[y], p = s[y], x = m.length, _ = (g[y] = new Array(x)), E, D = 0;
      D < x;
      ++D
    )
      (E = m[D] || p[D]) && (_[D] = E);
  for (; y < o; ++y) g[y] = i[y];
  return new Le(g, this._parents);
}
function tx() {
  for (var l = this._groups, u = -1, i = l.length; ++u < i; )
    for (var s = l[u], o = s.length - 1, f = s[o], d; --o >= 0; )
      (d = s[o]) &&
        (f &&
          d.compareDocumentPosition(f) ^ 4 &&
          f.parentNode.insertBefore(d, f),
        (f = d));
  return this;
}
function ex(l) {
  l || (l = nx);
  function u(x, _) {
    return x && _ ? l(x.__data__, _.__data__) : !x - !_;
  }
  for (
    var i = this._groups, s = i.length, o = new Array(s), f = 0;
    f < s;
    ++f
  ) {
    for (
      var d = i[f], g = d.length, y = (o[f] = new Array(g)), m, p = 0;
      p < g;
      ++p
    )
      (m = d[p]) && (y[p] = m);
    y.sort(u);
  }
  return new Le(o, this._parents).order();
}
function nx(l, u) {
  return l < u ? -1 : l > u ? 1 : l >= u ? 0 : NaN;
}
function lx() {
  var l = arguments[0];
  return ((arguments[0] = this), l.apply(null, arguments), this);
}
function ax() {
  return Array.from(this);
}
function ux() {
  for (var l = this._groups, u = 0, i = l.length; u < i; ++u)
    for (var s = l[u], o = 0, f = s.length; o < f; ++o) {
      var d = s[o];
      if (d) return d;
    }
  return null;
}
function ix() {
  let l = 0;
  for (const u of this) ++l;
  return l;
}
function cx() {
  return !this.node();
}
function sx(l) {
  for (var u = this._groups, i = 0, s = u.length; i < s; ++i)
    for (var o = u[i], f = 0, d = o.length, g; f < d; ++f)
      (g = o[f]) && l.call(g, g.__data__, f, o);
  return this;
}
function rx(l) {
  return function () {
    this.removeAttribute(l);
  };
}
function ox(l) {
  return function () {
    this.removeAttributeNS(l.space, l.local);
  };
}
function fx(l, u) {
  return function () {
    this.setAttribute(l, u);
  };
}
function dx(l, u) {
  return function () {
    this.setAttributeNS(l.space, l.local, u);
  };
}
function hx(l, u) {
  return function () {
    var i = u.apply(this, arguments);
    i == null ? this.removeAttribute(l) : this.setAttribute(l, i);
  };
}
function mx(l, u) {
  return function () {
    var i = u.apply(this, arguments);
    i == null
      ? this.removeAttributeNS(l.space, l.local)
      : this.setAttributeNS(l.space, l.local, i);
  };
}
function gx(l, u) {
  var i = cs(l);
  if (arguments.length < 2) {
    var s = this.node();
    return i.local ? s.getAttributeNS(i.space, i.local) : s.getAttribute(i);
  }
  return this.each(
    (u == null
      ? i.local
        ? ox
        : rx
      : typeof u == "function"
        ? i.local
          ? mx
          : hx
        : i.local
          ? dx
          : fx)(i, u),
  );
}
function Mg(l) {
  return (
    (l.ownerDocument && l.ownerDocument.defaultView) ||
    (l.document && l) ||
    l.defaultView
  );
}
function yx(l) {
  return function () {
    this.style.removeProperty(l);
  };
}
function px(l, u, i) {
  return function () {
    this.style.setProperty(l, u, i);
  };
}
function vx(l, u, i) {
  return function () {
    var s = u.apply(this, arguments);
    s == null ? this.style.removeProperty(l) : this.style.setProperty(l, s, i);
  };
}
function xx(l, u, i) {
  return arguments.length > 1
    ? this.each(
        (u == null ? yx : typeof u == "function" ? vx : px)(l, u, i ?? ""),
      )
    : $a(this.node(), l);
}
function $a(l, u) {
  return (
    l.style.getPropertyValue(u) ||
    Mg(l).getComputedStyle(l, null).getPropertyValue(u)
  );
}
function bx(l) {
  return function () {
    delete this[l];
  };
}
function Sx(l, u) {
  return function () {
    this[l] = u;
  };
}
function Ex(l, u) {
  return function () {
    var i = u.apply(this, arguments);
    i == null ? delete this[l] : (this[l] = i);
  };
}
function _x(l, u) {
  return arguments.length > 1
    ? this.each((u == null ? bx : typeof u == "function" ? Ex : Sx)(l, u))
    : this.node()[l];
}
function Cg(l) {
  return l.trim().split(/^|\s+/);
}
function df(l) {
  return l.classList || new Dg(l);
}
function Dg(l) {
  ((this._node = l), (this._names = Cg(l.getAttribute("class") || "")));
}
Dg.prototype = {
  add: function (l) {
    var u = this._names.indexOf(l);
    u < 0 &&
      (this._names.push(l),
      this._node.setAttribute("class", this._names.join(" ")));
  },
  remove: function (l) {
    var u = this._names.indexOf(l);
    u >= 0 &&
      (this._names.splice(u, 1),
      this._node.setAttribute("class", this._names.join(" ")));
  },
  contains: function (l) {
    return this._names.indexOf(l) >= 0;
  },
};
function Og(l, u) {
  for (var i = df(l), s = -1, o = u.length; ++s < o; ) i.add(u[s]);
}
function Rg(l, u) {
  for (var i = df(l), s = -1, o = u.length; ++s < o; ) i.remove(u[s]);
}
function wx(l) {
  return function () {
    Og(this, l);
  };
}
function Nx(l) {
  return function () {
    Rg(this, l);
  };
}
function Ax(l, u) {
  return function () {
    (u.apply(this, arguments) ? Og : Rg)(this, l);
  };
}
function Tx(l, u) {
  var i = Cg(l + "");
  if (arguments.length < 2) {
    for (var s = df(this.node()), o = -1, f = i.length; ++o < f; )
      if (!s.contains(i[o])) return !1;
    return !0;
  }
  return this.each((typeof u == "function" ? Ax : u ? wx : Nx)(i, u));
}
function zx() {
  this.textContent = "";
}
function Mx(l) {
  return function () {
    this.textContent = l;
  };
}
function Cx(l) {
  return function () {
    var u = l.apply(this, arguments);
    this.textContent = u ?? "";
  };
}
function Dx(l) {
  return arguments.length
    ? this.each(l == null ? zx : (typeof l == "function" ? Cx : Mx)(l))
    : this.node().textContent;
}
function Ox() {
  this.innerHTML = "";
}
function Rx(l) {
  return function () {
    this.innerHTML = l;
  };
}
function Hx(l) {
  return function () {
    var u = l.apply(this, arguments);
    this.innerHTML = u ?? "";
  };
}
function jx(l) {
  return arguments.length
    ? this.each(l == null ? Ox : (typeof l == "function" ? Hx : Rx)(l))
    : this.node().innerHTML;
}
function Ux() {
  this.nextSibling && this.parentNode.appendChild(this);
}
function Bx() {
  return this.each(Ux);
}
function Yx() {
  this.previousSibling &&
    this.parentNode.insertBefore(this, this.parentNode.firstChild);
}
function qx() {
  return this.each(Yx);
}
function Xx(l) {
  var u = typeof l == "function" ? l : wg(l);
  return this.select(function () {
    return this.appendChild(u.apply(this, arguments));
  });
}
function Vx() {
  return null;
}
function Lx(l, u) {
  var i = typeof l == "function" ? l : wg(l),
    s = u == null ? Vx : typeof u == "function" ? u : ff(u);
  return this.select(function () {
    return this.insertBefore(
      i.apply(this, arguments),
      s.apply(this, arguments) || null,
    );
  });
}
function Gx() {
  var l = this.parentNode;
  l && l.removeChild(this);
}
function Zx() {
  return this.each(Gx);
}
function Qx() {
  var l = this.cloneNode(!1),
    u = this.parentNode;
  return u ? u.insertBefore(l, this.nextSibling) : l;
}
function kx() {
  var l = this.cloneNode(!0),
    u = this.parentNode;
  return u ? u.insertBefore(l, this.nextSibling) : l;
}
function Kx(l) {
  return this.select(l ? kx : Qx);
}
function $x(l) {
  return arguments.length ? this.property("__data__", l) : this.node().__data__;
}
function Jx(l) {
  return function (u) {
    l.call(this, u, this.__data__);
  };
}
function Wx(l) {
  return l
    .trim()
    .split(/^|\s+/)
    .map(function (u) {
      var i = "",
        s = u.indexOf(".");
      return (
        s >= 0 && ((i = u.slice(s + 1)), (u = u.slice(0, s))),
        { type: u, name: i }
      );
    });
}
function Fx(l) {
  return function () {
    var u = this.__on;
    if (u) {
      for (var i = 0, s = -1, o = u.length, f; i < o; ++i)
        ((f = u[i]),
          (!l.type || f.type === l.type) && f.name === l.name
            ? this.removeEventListener(f.type, f.listener, f.options)
            : (u[++s] = f));
      ++s ? (u.length = s) : delete this.__on;
    }
  };
}
function Ix(l, u, i) {
  return function () {
    var s = this.__on,
      o,
      f = Jx(u);
    if (s) {
      for (var d = 0, g = s.length; d < g; ++d)
        if ((o = s[d]).type === l.type && o.name === l.name) {
          (this.removeEventListener(o.type, o.listener, o.options),
            this.addEventListener(o.type, (o.listener = f), (o.options = i)),
            (o.value = u));
          return;
        }
    }
    (this.addEventListener(l.type, f, i),
      (o = { type: l.type, name: l.name, value: u, listener: f, options: i }),
      s ? s.push(o) : (this.__on = [o]));
  };
}
function Px(l, u, i) {
  var s = Wx(l + ""),
    o,
    f = s.length,
    d;
  if (arguments.length < 2) {
    var g = this.node().__on;
    if (g) {
      for (var y = 0, m = g.length, p; y < m; ++y)
        for (o = 0, p = g[y]; o < f; ++o)
          if ((d = s[o]).type === p.type && d.name === p.name) return p.value;
    }
    return;
  }
  for (g = u ? Ix : Fx, o = 0; o < f; ++o) this.each(g(s[o], u, i));
  return this;
}
function Hg(l, u, i) {
  var s = Mg(l),
    o = s.CustomEvent;
  (typeof o == "function"
    ? (o = new o(u, i))
    : ((o = s.document.createEvent("Event")),
      i
        ? (o.initEvent(u, i.bubbles, i.cancelable), (o.detail = i.detail))
        : o.initEvent(u, !1, !1)),
    l.dispatchEvent(o));
}
function tb(l, u) {
  return function () {
    return Hg(this, l, u);
  };
}
function eb(l, u) {
  return function () {
    return Hg(this, l, u.apply(this, arguments));
  };
}
function nb(l, u) {
  return this.each((typeof u == "function" ? eb : tb)(l, u));
}
function* lb() {
  for (var l = this._groups, u = 0, i = l.length; u < i; ++u)
    for (var s = l[u], o = 0, f = s.length, d; o < f; ++o)
      (d = s[o]) && (yield d);
}
var jg = [null];
function Le(l, u) {
  ((this._groups = l), (this._parents = u));
}
function di() {
  return new Le([[document.documentElement]], jg);
}
function ab() {
  return this;
}
Le.prototype = di.prototype = {
  constructor: Le,
  select: C1,
  selectAll: H1,
  selectChild: Y1,
  selectChildren: L1,
  filter: G1,
  data: J1,
  enter: Z1,
  exit: F1,
  join: I1,
  merge: P1,
  selection: ab,
  order: tx,
  sort: ex,
  call: lx,
  nodes: ax,
  node: ux,
  size: ix,
  empty: cx,
  each: sx,
  attr: gx,
  style: xx,
  property: _x,
  classed: Tx,
  text: Dx,
  html: jx,
  raise: Bx,
  lower: qx,
  append: Xx,
  insert: Lx,
  remove: Zx,
  clone: Kx,
  datum: $x,
  on: Px,
  dispatch: nb,
  [Symbol.iterator]: lb,
};
function tn(l) {
  return typeof l == "string"
    ? new Le([[document.querySelector(l)]], [document.documentElement])
    : new Le([[l]], jg);
}
function ub(l) {
  let u;
  for (; (u = l.sourceEvent); ) l = u;
  return l;
}
function cn(l, u) {
  if (((l = ub(l)), u === void 0 && (u = l.currentTarget), u)) {
    var i = u.ownerSVGElement || u;
    if (i.createSVGPoint) {
      var s = i.createSVGPoint();
      return (
        (s.x = l.clientX),
        (s.y = l.clientY),
        (s = s.matrixTransform(u.getScreenCTM().inverse())),
        [s.x, s.y]
      );
    }
    if (u.getBoundingClientRect) {
      var o = u.getBoundingClientRect();
      return [
        l.clientX - o.left - u.clientLeft,
        l.clientY - o.top - u.clientTop,
      ];
    }
  }
  return [l.pageX, l.pageY];
}
const ib = { passive: !1 },
  li = { capture: !0, passive: !1 };
function Bo(l) {
  l.stopImmediatePropagation();
}
function Qa(l) {
  (l.preventDefault(), l.stopImmediatePropagation());
}
function Ug(l) {
  var u = l.document.documentElement,
    i = tn(l).on("dragstart.drag", Qa, li);
  "onselectstart" in u
    ? i.on("selectstart.drag", Qa, li)
    : ((u.__noselect = u.style.MozUserSelect),
      (u.style.MozUserSelect = "none"));
}
function Bg(l, u) {
  var i = l.document.documentElement,
    s = tn(l).on("dragstart.drag", null);
  (u &&
    (s.on("click.drag", Qa, li),
    setTimeout(function () {
      s.on("click.drag", null);
    }, 0)),
    "onselectstart" in i
      ? s.on("selectstart.drag", null)
      : ((i.style.MozUserSelect = i.__noselect), delete i.__noselect));
}
const Yc = (l) => () => l;
function $o(
  l,
  {
    sourceEvent: u,
    subject: i,
    target: s,
    identifier: o,
    active: f,
    x: d,
    y: g,
    dx: y,
    dy: m,
    dispatch: p,
  },
) {
  Object.defineProperties(this, {
    type: { value: l, enumerable: !0, configurable: !0 },
    sourceEvent: { value: u, enumerable: !0, configurable: !0 },
    subject: { value: i, enumerable: !0, configurable: !0 },
    target: { value: s, enumerable: !0, configurable: !0 },
    identifier: { value: o, enumerable: !0, configurable: !0 },
    active: { value: f, enumerable: !0, configurable: !0 },
    x: { value: d, enumerable: !0, configurable: !0 },
    y: { value: g, enumerable: !0, configurable: !0 },
    dx: { value: y, enumerable: !0, configurable: !0 },
    dy: { value: m, enumerable: !0, configurable: !0 },
    _: { value: p },
  });
}
$o.prototype.on = function () {
  var l = this._.on.apply(this._, arguments);
  return l === this._ ? this : l;
};
function cb(l) {
  return !l.ctrlKey && !l.button;
}
function sb() {
  return this.parentNode;
}
function rb(l, u) {
  return u ?? { x: l.x, y: l.y };
}
function ob() {
  return navigator.maxTouchPoints || "ontouchstart" in this;
}
function fb() {
  var l = cb,
    u = sb,
    i = rb,
    s = ob,
    o = {},
    f = is("start", "drag", "end"),
    d = 0,
    g,
    y,
    m,
    p,
    x = 0;
  function _(j) {
    j.on("mousedown.drag", E)
      .filter(s)
      .on("touchstart.drag", B)
      .on("touchmove.drag", N, ib)
      .on("touchend.drag touchcancel.drag", Y)
      .style("touch-action", "none")
      .style("-webkit-tap-highlight-color", "rgba(0,0,0,0)");
  }
  function E(j, W) {
    if (!(p || !l.call(this, j, W))) {
      var I = G(this, u.call(this, j, W), j, W, "mouse");
      I &&
        (tn(j.view).on("mousemove.drag", D, li).on("mouseup.drag", z, li),
        Ug(j.view),
        Bo(j),
        (m = !1),
        (g = j.clientX),
        (y = j.clientY),
        I("start", j));
    }
  }
  function D(j) {
    if ((Qa(j), !m)) {
      var W = j.clientX - g,
        I = j.clientY - y;
      m = W * W + I * I > x;
    }
    o.mouse("drag", j);
  }
  function z(j) {
    (tn(j.view).on("mousemove.drag mouseup.drag", null),
      Bg(j.view, m),
      Qa(j),
      o.mouse("end", j));
  }
  function B(j, W) {
    if (l.call(this, j, W)) {
      var I = j.changedTouches,
        Q = u.call(this, j, W),
        ut = I.length,
        et,
        ct;
      for (et = 0; et < ut; ++et)
        (ct = G(this, Q, j, W, I[et].identifier, I[et])) &&
          (Bo(j), ct("start", j, I[et]));
    }
  }
  function N(j) {
    var W = j.changedTouches,
      I = W.length,
      Q,
      ut;
    for (Q = 0; Q < I; ++Q)
      (ut = o[W[Q].identifier]) && (Qa(j), ut("drag", j, W[Q]));
  }
  function Y(j) {
    var W = j.changedTouches,
      I = W.length,
      Q,
      ut;
    for (
      p && clearTimeout(p),
        p = setTimeout(function () {
          p = null;
        }, 500),
        Q = 0;
      Q < I;
      ++Q
    )
      (ut = o[W[Q].identifier]) && (Bo(j), ut("end", j, W[Q]));
  }
  function G(j, W, I, Q, ut, et) {
    var ct = f.copy(),
      at = cn(et || I, W),
      st,
      rt,
      w;
    if (
      (w = i.call(
        j,
        new $o("beforestart", {
          sourceEvent: I,
          target: _,
          identifier: ut,
          active: d,
          x: at[0],
          y: at[1],
          dx: 0,
          dy: 0,
          dispatch: ct,
        }),
        Q,
      )) != null
    )
      return (
        (st = w.x - at[0] || 0),
        (rt = w.y - at[1] || 0),
        function L(S, R, Z) {
          var O = at,
            k;
          switch (S) {
            case "start":
              ((o[ut] = L), (k = d++));
              break;
            case "end":
              (delete o[ut], --d);
            case "drag":
              ((at = cn(Z || R, W)), (k = d));
              break;
          }
          ct.call(
            S,
            j,
            new $o(S, {
              sourceEvent: R,
              subject: w,
              target: _,
              identifier: ut,
              active: k,
              x: at[0] + st,
              y: at[1] + rt,
              dx: at[0] - O[0],
              dy: at[1] - O[1],
              dispatch: ct,
            }),
            Q,
          );
        }
      );
  }
  return (
    (_.filter = function (j) {
      return arguments.length
        ? ((l = typeof j == "function" ? j : Yc(!!j)), _)
        : l;
    }),
    (_.container = function (j) {
      return arguments.length
        ? ((u = typeof j == "function" ? j : Yc(j)), _)
        : u;
    }),
    (_.subject = function (j) {
      return arguments.length
        ? ((i = typeof j == "function" ? j : Yc(j)), _)
        : i;
    }),
    (_.touchable = function (j) {
      return arguments.length
        ? ((s = typeof j == "function" ? j : Yc(!!j)), _)
        : s;
    }),
    (_.on = function () {
      var j = f.on.apply(f, arguments);
      return j === f ? _ : j;
    }),
    (_.clickDistance = function (j) {
      return arguments.length ? ((x = (j = +j) * j), _) : Math.sqrt(x);
    }),
    _
  );
}
function hf(l, u, i) {
  ((l.prototype = u.prototype = i), (i.constructor = l));
}
function Yg(l, u) {
  var i = Object.create(l.prototype);
  for (var s in u) i[s] = u[s];
  return i;
}
function hi() {}
var ai = 0.7,
  Ic = 1 / ai,
  ka = "\\s*([+-]?\\d+)\\s*",
  ui = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)\\s*",
  xn = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)%\\s*",
  db = /^#([0-9a-f]{3,8})$/,
  hb = new RegExp(`^rgb\\(${ka},${ka},${ka}\\)$`),
  mb = new RegExp(`^rgb\\(${xn},${xn},${xn}\\)$`),
  gb = new RegExp(`^rgba\\(${ka},${ka},${ka},${ui}\\)$`),
  yb = new RegExp(`^rgba\\(${xn},${xn},${xn},${ui}\\)$`),
  pb = new RegExp(`^hsl\\(${ui},${xn},${xn}\\)$`),
  vb = new RegExp(`^hsla\\(${ui},${xn},${xn},${ui}\\)$`),
  O0 = {
    aliceblue: 15792383,
    antiquewhite: 16444375,
    aqua: 65535,
    aquamarine: 8388564,
    azure: 15794175,
    beige: 16119260,
    bisque: 16770244,
    black: 0,
    blanchedalmond: 16772045,
    blue: 255,
    blueviolet: 9055202,
    brown: 10824234,
    burlywood: 14596231,
    cadetblue: 6266528,
    chartreuse: 8388352,
    chocolate: 13789470,
    coral: 16744272,
    cornflowerblue: 6591981,
    cornsilk: 16775388,
    crimson: 14423100,
    cyan: 65535,
    darkblue: 139,
    darkcyan: 35723,
    darkgoldenrod: 12092939,
    darkgray: 11119017,
    darkgreen: 25600,
    darkgrey: 11119017,
    darkkhaki: 12433259,
    darkmagenta: 9109643,
    darkolivegreen: 5597999,
    darkorange: 16747520,
    darkorchid: 10040012,
    darkred: 9109504,
    darksalmon: 15308410,
    darkseagreen: 9419919,
    darkslateblue: 4734347,
    darkslategray: 3100495,
    darkslategrey: 3100495,
    darkturquoise: 52945,
    darkviolet: 9699539,
    deeppink: 16716947,
    deepskyblue: 49151,
    dimgray: 6908265,
    dimgrey: 6908265,
    dodgerblue: 2003199,
    firebrick: 11674146,
    floralwhite: 16775920,
    forestgreen: 2263842,
    fuchsia: 16711935,
    gainsboro: 14474460,
    ghostwhite: 16316671,
    gold: 16766720,
    goldenrod: 14329120,
    gray: 8421504,
    green: 32768,
    greenyellow: 11403055,
    grey: 8421504,
    honeydew: 15794160,
    hotpink: 16738740,
    indianred: 13458524,
    indigo: 4915330,
    ivory: 16777200,
    khaki: 15787660,
    lavender: 15132410,
    lavenderblush: 16773365,
    lawngreen: 8190976,
    lemonchiffon: 16775885,
    lightblue: 11393254,
    lightcoral: 15761536,
    lightcyan: 14745599,
    lightgoldenrodyellow: 16448210,
    lightgray: 13882323,
    lightgreen: 9498256,
    lightgrey: 13882323,
    lightpink: 16758465,
    lightsalmon: 16752762,
    lightseagreen: 2142890,
    lightskyblue: 8900346,
    lightslategray: 7833753,
    lightslategrey: 7833753,
    lightsteelblue: 11584734,
    lightyellow: 16777184,
    lime: 65280,
    limegreen: 3329330,
    linen: 16445670,
    magenta: 16711935,
    maroon: 8388608,
    mediumaquamarine: 6737322,
    mediumblue: 205,
    mediumorchid: 12211667,
    mediumpurple: 9662683,
    mediumseagreen: 3978097,
    mediumslateblue: 8087790,
    mediumspringgreen: 64154,
    mediumturquoise: 4772300,
    mediumvioletred: 13047173,
    midnightblue: 1644912,
    mintcream: 16121850,
    mistyrose: 16770273,
    moccasin: 16770229,
    navajowhite: 16768685,
    navy: 128,
    oldlace: 16643558,
    olive: 8421376,
    olivedrab: 7048739,
    orange: 16753920,
    orangered: 16729344,
    orchid: 14315734,
    palegoldenrod: 15657130,
    palegreen: 10025880,
    paleturquoise: 11529966,
    palevioletred: 14381203,
    papayawhip: 16773077,
    peachpuff: 16767673,
    peru: 13468991,
    pink: 16761035,
    plum: 14524637,
    powderblue: 11591910,
    purple: 8388736,
    rebeccapurple: 6697881,
    red: 16711680,
    rosybrown: 12357519,
    royalblue: 4286945,
    saddlebrown: 9127187,
    salmon: 16416882,
    sandybrown: 16032864,
    seagreen: 3050327,
    seashell: 16774638,
    sienna: 10506797,
    silver: 12632256,
    skyblue: 8900331,
    slateblue: 6970061,
    slategray: 7372944,
    slategrey: 7372944,
    snow: 16775930,
    springgreen: 65407,
    steelblue: 4620980,
    tan: 13808780,
    teal: 32896,
    thistle: 14204888,
    tomato: 16737095,
    turquoise: 4251856,
    violet: 15631086,
    wheat: 16113331,
    white: 16777215,
    whitesmoke: 16119285,
    yellow: 16776960,
    yellowgreen: 10145074,
  };
hf(hi, ii, {
  copy(l) {
    return Object.assign(new this.constructor(), this, l);
  },
  displayable() {
    return this.rgb().displayable();
  },
  hex: R0,
  formatHex: R0,
  formatHex8: xb,
  formatHsl: bb,
  formatRgb: H0,
  toString: H0,
});
function R0() {
  return this.rgb().formatHex();
}
function xb() {
  return this.rgb().formatHex8();
}
function bb() {
  return qg(this).formatHsl();
}
function H0() {
  return this.rgb().formatRgb();
}
function ii(l) {
  var u, i;
  return (
    (l = (l + "").trim().toLowerCase()),
    (u = db.exec(l))
      ? ((i = u[1].length),
        (u = parseInt(u[1], 16)),
        i === 6
          ? j0(u)
          : i === 3
            ? new De(
                ((u >> 8) & 15) | ((u >> 4) & 240),
                ((u >> 4) & 15) | (u & 240),
                ((u & 15) << 4) | (u & 15),
                1,
              )
            : i === 8
              ? qc(
                  (u >> 24) & 255,
                  (u >> 16) & 255,
                  (u >> 8) & 255,
                  (u & 255) / 255,
                )
              : i === 4
                ? qc(
                    ((u >> 12) & 15) | ((u >> 8) & 240),
                    ((u >> 8) & 15) | ((u >> 4) & 240),
                    ((u >> 4) & 15) | (u & 240),
                    (((u & 15) << 4) | (u & 15)) / 255,
                  )
                : null)
      : (u = hb.exec(l))
        ? new De(u[1], u[2], u[3], 1)
        : (u = mb.exec(l))
          ? new De(
              (u[1] * 255) / 100,
              (u[2] * 255) / 100,
              (u[3] * 255) / 100,
              1,
            )
          : (u = gb.exec(l))
            ? qc(u[1], u[2], u[3], u[4])
            : (u = yb.exec(l))
              ? qc(
                  (u[1] * 255) / 100,
                  (u[2] * 255) / 100,
                  (u[3] * 255) / 100,
                  u[4],
                )
              : (u = pb.exec(l))
                ? Y0(u[1], u[2] / 100, u[3] / 100, 1)
                : (u = vb.exec(l))
                  ? Y0(u[1], u[2] / 100, u[3] / 100, u[4])
                  : O0.hasOwnProperty(l)
                    ? j0(O0[l])
                    : l === "transparent"
                      ? new De(NaN, NaN, NaN, 0)
                      : null
  );
}
function j0(l) {
  return new De((l >> 16) & 255, (l >> 8) & 255, l & 255, 1);
}
function qc(l, u, i, s) {
  return (s <= 0 && (l = u = i = NaN), new De(l, u, i, s));
}
function Sb(l) {
  return (
    l instanceof hi || (l = ii(l)),
    l ? ((l = l.rgb()), new De(l.r, l.g, l.b, l.opacity)) : new De()
  );
}
function Jo(l, u, i, s) {
  return arguments.length === 1 ? Sb(l) : new De(l, u, i, s ?? 1);
}
function De(l, u, i, s) {
  ((this.r = +l), (this.g = +u), (this.b = +i), (this.opacity = +s));
}
hf(
  De,
  Jo,
  Yg(hi, {
    brighter(l) {
      return (
        (l = l == null ? Ic : Math.pow(Ic, l)),
        new De(this.r * l, this.g * l, this.b * l, this.opacity)
      );
    },
    darker(l) {
      return (
        (l = l == null ? ai : Math.pow(ai, l)),
        new De(this.r * l, this.g * l, this.b * l, this.opacity)
      );
    },
    rgb() {
      return this;
    },
    clamp() {
      return new De(na(this.r), na(this.g), na(this.b), Pc(this.opacity));
    },
    displayable() {
      return (
        -0.5 <= this.r &&
        this.r < 255.5 &&
        -0.5 <= this.g &&
        this.g < 255.5 &&
        -0.5 <= this.b &&
        this.b < 255.5 &&
        0 <= this.opacity &&
        this.opacity <= 1
      );
    },
    hex: U0,
    formatHex: U0,
    formatHex8: Eb,
    formatRgb: B0,
    toString: B0,
  }),
);
function U0() {
  return `#${ta(this.r)}${ta(this.g)}${ta(this.b)}`;
}
function Eb() {
  return `#${ta(this.r)}${ta(this.g)}${ta(this.b)}${ta((isNaN(this.opacity) ? 1 : this.opacity) * 255)}`;
}
function B0() {
  const l = Pc(this.opacity);
  return `${l === 1 ? "rgb(" : "rgba("}${na(this.r)}, ${na(this.g)}, ${na(this.b)}${l === 1 ? ")" : `, ${l})`}`;
}
function Pc(l) {
  return isNaN(l) ? 1 : Math.max(0, Math.min(1, l));
}
function na(l) {
  return Math.max(0, Math.min(255, Math.round(l) || 0));
}
function ta(l) {
  return ((l = na(l)), (l < 16 ? "0" : "") + l.toString(16));
}
function Y0(l, u, i, s) {
  return (
    s <= 0
      ? (l = u = i = NaN)
      : i <= 0 || i >= 1
        ? (l = u = NaN)
        : u <= 0 && (l = NaN),
    new sn(l, u, i, s)
  );
}
function qg(l) {
  if (l instanceof sn) return new sn(l.h, l.s, l.l, l.opacity);
  if ((l instanceof hi || (l = ii(l)), !l)) return new sn();
  if (l instanceof sn) return l;
  l = l.rgb();
  var u = l.r / 255,
    i = l.g / 255,
    s = l.b / 255,
    o = Math.min(u, i, s),
    f = Math.max(u, i, s),
    d = NaN,
    g = f - o,
    y = (f + o) / 2;
  return (
    g
      ? (u === f
          ? (d = (i - s) / g + (i < s) * 6)
          : i === f
            ? (d = (s - u) / g + 2)
            : (d = (u - i) / g + 4),
        (g /= y < 0.5 ? f + o : 2 - f - o),
        (d *= 60))
      : (g = y > 0 && y < 1 ? 0 : d),
    new sn(d, g, y, l.opacity)
  );
}
function _b(l, u, i, s) {
  return arguments.length === 1 ? qg(l) : new sn(l, u, i, s ?? 1);
}
function sn(l, u, i, s) {
  ((this.h = +l), (this.s = +u), (this.l = +i), (this.opacity = +s));
}
hf(
  sn,
  _b,
  Yg(hi, {
    brighter(l) {
      return (
        (l = l == null ? Ic : Math.pow(Ic, l)),
        new sn(this.h, this.s, this.l * l, this.opacity)
      );
    },
    darker(l) {
      return (
        (l = l == null ? ai : Math.pow(ai, l)),
        new sn(this.h, this.s, this.l * l, this.opacity)
      );
    },
    rgb() {
      var l = (this.h % 360) + (this.h < 0) * 360,
        u = isNaN(l) || isNaN(this.s) ? 0 : this.s,
        i = this.l,
        s = i + (i < 0.5 ? i : 1 - i) * u,
        o = 2 * i - s;
      return new De(
        Yo(l >= 240 ? l - 240 : l + 120, o, s),
        Yo(l, o, s),
        Yo(l < 120 ? l + 240 : l - 120, o, s),
        this.opacity,
      );
    },
    clamp() {
      return new sn(q0(this.h), Xc(this.s), Xc(this.l), Pc(this.opacity));
    },
    displayable() {
      return (
        ((0 <= this.s && this.s <= 1) || isNaN(this.s)) &&
        0 <= this.l &&
        this.l <= 1 &&
        0 <= this.opacity &&
        this.opacity <= 1
      );
    },
    formatHsl() {
      const l = Pc(this.opacity);
      return `${l === 1 ? "hsl(" : "hsla("}${q0(this.h)}, ${Xc(this.s) * 100}%, ${Xc(this.l) * 100}%${l === 1 ? ")" : `, ${l})`}`;
    },
  }),
);
function q0(l) {
  return ((l = (l || 0) % 360), l < 0 ? l + 360 : l);
}
function Xc(l) {
  return Math.max(0, Math.min(1, l || 0));
}
function Yo(l, u, i) {
  return (
    (l < 60
      ? u + ((i - u) * l) / 60
      : l < 180
        ? i
        : l < 240
          ? u + ((i - u) * (240 - l)) / 60
          : u) * 255
  );
}
const Xg = (l) => () => l;
function wb(l, u) {
  return function (i) {
    return l + i * u;
  };
}
function Nb(l, u, i) {
  return (
    (l = Math.pow(l, i)),
    (u = Math.pow(u, i) - l),
    (i = 1 / i),
    function (s) {
      return Math.pow(l + s * u, i);
    }
  );
}
function Ab(l) {
  return (l = +l) == 1
    ? Vg
    : function (u, i) {
        return i - u ? Nb(u, i, l) : Xg(isNaN(u) ? i : u);
      };
}
function Vg(l, u) {
  var i = u - l;
  return i ? wb(l, i) : Xg(isNaN(l) ? u : l);
}
const X0 = (function l(u) {
  var i = Ab(u);
  function s(o, f) {
    var d = i((o = Jo(o)).r, (f = Jo(f)).r),
      g = i(o.g, f.g),
      y = i(o.b, f.b),
      m = Vg(o.opacity, f.opacity);
    return function (p) {
      return (
        (o.r = d(p)),
        (o.g = g(p)),
        (o.b = y(p)),
        (o.opacity = m(p)),
        o + ""
      );
    };
  }
  return ((s.gamma = l), s);
})(1);
function Tl(l, u) {
  return (
    (l = +l),
    (u = +u),
    function (i) {
      return l * (1 - i) + u * i;
    }
  );
}
var Wo = /[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g,
  qo = new RegExp(Wo.source, "g");
function Tb(l) {
  return function () {
    return l;
  };
}
function zb(l) {
  return function (u) {
    return l(u) + "";
  };
}
function Mb(l, u) {
  var i = (Wo.lastIndex = qo.lastIndex = 0),
    s,
    o,
    f,
    d = -1,
    g = [],
    y = [];
  for (l = l + "", u = u + ""; (s = Wo.exec(l)) && (o = qo.exec(u)); )
    ((f = o.index) > i &&
      ((f = u.slice(i, f)), g[d] ? (g[d] += f) : (g[++d] = f)),
      (s = s[0]) === (o = o[0])
        ? g[d]
          ? (g[d] += o)
          : (g[++d] = o)
        : ((g[++d] = null), y.push({ i: d, x: Tl(s, o) })),
      (i = qo.lastIndex));
  return (
    i < u.length && ((f = u.slice(i)), g[d] ? (g[d] += f) : (g[++d] = f)),
    g.length < 2
      ? y[0]
        ? zb(y[0].x)
        : Tb(u)
      : ((u = y.length),
        function (m) {
          for (var p = 0, x; p < u; ++p) g[(x = y[p]).i] = x.x(m);
          return g.join("");
        })
  );
}
var V0 = 180 / Math.PI,
  Fo = {
    translateX: 0,
    translateY: 0,
    rotate: 0,
    skewX: 0,
    scaleX: 1,
    scaleY: 1,
  };
function Lg(l, u, i, s, o, f) {
  var d, g, y;
  return (
    (d = Math.sqrt(l * l + u * u)) && ((l /= d), (u /= d)),
    (y = l * i + u * s) && ((i -= l * y), (s -= u * y)),
    (g = Math.sqrt(i * i + s * s)) && ((i /= g), (s /= g), (y /= g)),
    l * s < u * i && ((l = -l), (u = -u), (y = -y), (d = -d)),
    {
      translateX: o,
      translateY: f,
      rotate: Math.atan2(u, l) * V0,
      skewX: Math.atan(y) * V0,
      scaleX: d,
      scaleY: g,
    }
  );
}
var Vc;
function Cb(l) {
  const u = new (typeof DOMMatrix == "function" ? DOMMatrix : WebKitCSSMatrix)(
    l + "",
  );
  return u.isIdentity ? Fo : Lg(u.a, u.b, u.c, u.d, u.e, u.f);
}
function Db(l) {
  return l == null ||
    (Vc || (Vc = document.createElementNS("http://www.w3.org/2000/svg", "g")),
    Vc.setAttribute("transform", l),
    !(l = Vc.transform.baseVal.consolidate()))
    ? Fo
    : ((l = l.matrix), Lg(l.a, l.b, l.c, l.d, l.e, l.f));
}
function Gg(l, u, i, s) {
  function o(m) {
    return m.length ? m.pop() + " " : "";
  }
  function f(m, p, x, _, E, D) {
    if (m !== x || p !== _) {
      var z = E.push("translate(", null, u, null, i);
      D.push({ i: z - 4, x: Tl(m, x) }, { i: z - 2, x: Tl(p, _) });
    } else (x || _) && E.push("translate(" + x + u + _ + i);
  }
  function d(m, p, x, _) {
    m !== p
      ? (m - p > 180 ? (p += 360) : p - m > 180 && (m += 360),
        _.push({ i: x.push(o(x) + "rotate(", null, s) - 2, x: Tl(m, p) }))
      : p && x.push(o(x) + "rotate(" + p + s);
  }
  function g(m, p, x, _) {
    m !== p
      ? _.push({ i: x.push(o(x) + "skewX(", null, s) - 2, x: Tl(m, p) })
      : p && x.push(o(x) + "skewX(" + p + s);
  }
  function y(m, p, x, _, E, D) {
    if (m !== x || p !== _) {
      var z = E.push(o(E) + "scale(", null, ",", null, ")");
      D.push({ i: z - 4, x: Tl(m, x) }, { i: z - 2, x: Tl(p, _) });
    } else (x !== 1 || _ !== 1) && E.push(o(E) + "scale(" + x + "," + _ + ")");
  }
  return function (m, p) {
    var x = [],
      _ = [];
    return (
      (m = l(m)),
      (p = l(p)),
      f(m.translateX, m.translateY, p.translateX, p.translateY, x, _),
      d(m.rotate, p.rotate, x, _),
      g(m.skewX, p.skewX, x, _),
      y(m.scaleX, m.scaleY, p.scaleX, p.scaleY, x, _),
      (m = p = null),
      function (E) {
        for (var D = -1, z = _.length, B; ++D < z; ) x[(B = _[D]).i] = B.x(E);
        return x.join("");
      }
    );
  };
}
var Ob = Gg(Cb, "px, ", "px)", "deg)"),
  Rb = Gg(Db, ", ", ")", ")"),
  Hb = 1e-12;
function L0(l) {
  return ((l = Math.exp(l)) + 1 / l) / 2;
}
function jb(l) {
  return ((l = Math.exp(l)) - 1 / l) / 2;
}
function Ub(l) {
  return ((l = Math.exp(2 * l)) - 1) / (l + 1);
}
const Bb = (function l(u, i, s) {
  function o(f, d) {
    var g = f[0],
      y = f[1],
      m = f[2],
      p = d[0],
      x = d[1],
      _ = d[2],
      E = p - g,
      D = x - y,
      z = E * E + D * D,
      B,
      N;
    if (z < Hb)
      ((N = Math.log(_ / m) / u),
        (B = function (Q) {
          return [g + Q * E, y + Q * D, m * Math.exp(u * Q * N)];
        }));
    else {
      var Y = Math.sqrt(z),
        G = (_ * _ - m * m + s * z) / (2 * m * i * Y),
        j = (_ * _ - m * m - s * z) / (2 * _ * i * Y),
        W = Math.log(Math.sqrt(G * G + 1) - G),
        I = Math.log(Math.sqrt(j * j + 1) - j);
      ((N = (I - W) / u),
        (B = function (Q) {
          var ut = Q * N,
            et = L0(W),
            ct = (m / (i * Y)) * (et * Ub(u * ut + W) - jb(W));
          return [g + ct * E, y + ct * D, (m * et) / L0(u * ut + W)];
        }));
    }
    return ((B.duration = (N * 1e3 * u) / Math.SQRT2), B);
  }
  return (
    (o.rho = function (f) {
      var d = Math.max(0.001, +f),
        g = d * d,
        y = g * g;
      return l(d, g, y);
    }),
    o
  );
})(Math.SQRT2, 2, 4);
var Ja = 0,
  ti = 0,
  Ju = 0,
  Zg = 1e3,
  ts,
  ei,
  es = 0,
  aa = 0,
  ss = 0,
  ci = typeof performance == "object" && performance.now ? performance : Date,
  Qg =
    typeof window == "object" && window.requestAnimationFrame
      ? window.requestAnimationFrame.bind(window)
      : function (l) {
          setTimeout(l, 17);
        };
function mf() {
  return aa || (Qg(Yb), (aa = ci.now() + ss));
}
function Yb() {
  aa = 0;
}
function ns() {
  this._call = this._time = this._next = null;
}
ns.prototype = kg.prototype = {
  constructor: ns,
  restart: function (l, u, i) {
    if (typeof l != "function")
      throw new TypeError("callback is not a function");
    ((i = (i == null ? mf() : +i) + (u == null ? 0 : +u)),
      !this._next &&
        ei !== this &&
        (ei ? (ei._next = this) : (ts = this), (ei = this)),
      (this._call = l),
      (this._time = i),
      Io());
  },
  stop: function () {
    this._call && ((this._call = null), (this._time = 1 / 0), Io());
  },
};
function kg(l, u, i) {
  var s = new ns();
  return (s.restart(l, u, i), s);
}
function qb() {
  (mf(), ++Ja);
  for (var l = ts, u; l; )
    ((u = aa - l._time) >= 0 && l._call.call(void 0, u), (l = l._next));
  --Ja;
}
function G0() {
  ((aa = (es = ci.now()) + ss), (Ja = ti = 0));
  try {
    qb();
  } finally {
    ((Ja = 0), Vb(), (aa = 0));
  }
}
function Xb() {
  var l = ci.now(),
    u = l - es;
  u > Zg && ((ss -= u), (es = l));
}
function Vb() {
  for (var l, u = ts, i, s = 1 / 0; u; )
    u._call
      ? (s > u._time && (s = u._time), (l = u), (u = u._next))
      : ((i = u._next), (u._next = null), (u = l ? (l._next = i) : (ts = i)));
  ((ei = l), Io(s));
}
function Io(l) {
  if (!Ja) {
    ti && (ti = clearTimeout(ti));
    var u = l - aa;
    u > 24
      ? (l < 1 / 0 && (ti = setTimeout(G0, l - ci.now() - ss)),
        Ju && (Ju = clearInterval(Ju)))
      : (Ju || ((es = ci.now()), (Ju = setInterval(Xb, Zg))), (Ja = 1), Qg(G0));
  }
}
function Z0(l, u, i) {
  var s = new ns();
  return (
    (u = u == null ? 0 : +u),
    s.restart(
      (o) => {
        (s.stop(), l(o + u));
      },
      u,
      i,
    ),
    s
  );
}
var Lb = is("start", "end", "cancel", "interrupt"),
  Gb = [],
  Kg = 0,
  Q0 = 1,
  Po = 2,
  $c = 3,
  k0 = 4,
  tf = 5,
  Jc = 6;
function rs(l, u, i, s, o, f) {
  var d = l.__transition;
  if (!d) l.__transition = {};
  else if (i in d) return;
  Zb(l, i, {
    name: u,
    index: s,
    group: o,
    on: Lb,
    tween: Gb,
    time: f.time,
    delay: f.delay,
    duration: f.duration,
    ease: f.ease,
    timer: null,
    state: Kg,
  });
}
function gf(l, u) {
  var i = on(l, u);
  if (i.state > Kg) throw new Error("too late; already scheduled");
  return i;
}
function bn(l, u) {
  var i = on(l, u);
  if (i.state > $c) throw new Error("too late; already running");
  return i;
}
function on(l, u) {
  var i = l.__transition;
  if (!i || !(i = i[u])) throw new Error("transition not found");
  return i;
}
function Zb(l, u, i) {
  var s = l.__transition,
    o;
  ((s[u] = i), (i.timer = kg(f, 0, i.time)));
  function f(m) {
    ((i.state = Q0),
      i.timer.restart(d, i.delay, i.time),
      i.delay <= m && d(m - i.delay));
  }
  function d(m) {
    var p, x, _, E;
    if (i.state !== Q0) return y();
    for (p in s)
      if (((E = s[p]), E.name === i.name)) {
        if (E.state === $c) return Z0(d);
        E.state === k0
          ? ((E.state = Jc),
            E.timer.stop(),
            E.on.call("interrupt", l, l.__data__, E.index, E.group),
            delete s[p])
          : +p < u &&
            ((E.state = Jc),
            E.timer.stop(),
            E.on.call("cancel", l, l.__data__, E.index, E.group),
            delete s[p]);
      }
    if (
      (Z0(function () {
        i.state === $c &&
          ((i.state = k0), i.timer.restart(g, i.delay, i.time), g(m));
      }),
      (i.state = Po),
      i.on.call("start", l, l.__data__, i.index, i.group),
      i.state === Po)
    ) {
      for (
        i.state = $c, o = new Array((_ = i.tween.length)), p = 0, x = -1;
        p < _;
        ++p
      )
        (E = i.tween[p].value.call(l, l.__data__, i.index, i.group)) &&
          (o[++x] = E);
      o.length = x + 1;
    }
  }
  function g(m) {
    for (
      var p =
          m < i.duration
            ? i.ease.call(null, m / i.duration)
            : (i.timer.restart(y), (i.state = tf), 1),
        x = -1,
        _ = o.length;
      ++x < _;
    )
      o[x].call(l, p);
    i.state === tf && (i.on.call("end", l, l.__data__, i.index, i.group), y());
  }
  function y() {
    ((i.state = Jc), i.timer.stop(), delete s[u]);
    for (var m in s) return;
    delete l.__transition;
  }
}
function Wc(l, u) {
  var i = l.__transition,
    s,
    o,
    f = !0,
    d;
  if (i) {
    u = u == null ? null : u + "";
    for (d in i) {
      if ((s = i[d]).name !== u) {
        f = !1;
        continue;
      }
      ((o = s.state > Po && s.state < tf),
        (s.state = Jc),
        s.timer.stop(),
        s.on.call(o ? "interrupt" : "cancel", l, l.__data__, s.index, s.group),
        delete i[d]);
    }
    f && delete l.__transition;
  }
}
function Qb(l) {
  return this.each(function () {
    Wc(this, l);
  });
}
function kb(l, u) {
  var i, s;
  return function () {
    var o = bn(this, l),
      f = o.tween;
    if (f !== i) {
      s = i = f;
      for (var d = 0, g = s.length; d < g; ++d)
        if (s[d].name === u) {
          ((s = s.slice()), s.splice(d, 1));
          break;
        }
    }
    o.tween = s;
  };
}
function Kb(l, u, i) {
  var s, o;
  if (typeof i != "function") throw new Error();
  return function () {
    var f = bn(this, l),
      d = f.tween;
    if (d !== s) {
      o = (s = d).slice();
      for (var g = { name: u, value: i }, y = 0, m = o.length; y < m; ++y)
        if (o[y].name === u) {
          o[y] = g;
          break;
        }
      y === m && o.push(g);
    }
    f.tween = o;
  };
}
function $b(l, u) {
  var i = this._id;
  if (((l += ""), arguments.length < 2)) {
    for (var s = on(this.node(), i).tween, o = 0, f = s.length, d; o < f; ++o)
      if ((d = s[o]).name === l) return d.value;
    return null;
  }
  return this.each((u == null ? kb : Kb)(i, l, u));
}
function yf(l, u, i) {
  var s = l._id;
  return (
    l.each(function () {
      var o = bn(this, s);
      (o.value || (o.value = {}))[u] = i.apply(this, arguments);
    }),
    function (o) {
      return on(o, s).value[u];
    }
  );
}
function $g(l, u) {
  var i;
  return (
    typeof u == "number"
      ? Tl
      : u instanceof ii
        ? X0
        : (i = ii(u))
          ? ((u = i), X0)
          : Mb
  )(l, u);
}
function Jb(l) {
  return function () {
    this.removeAttribute(l);
  };
}
function Wb(l) {
  return function () {
    this.removeAttributeNS(l.space, l.local);
  };
}
function Fb(l, u, i) {
  var s,
    o = i + "",
    f;
  return function () {
    var d = this.getAttribute(l);
    return d === o ? null : d === s ? f : (f = u((s = d), i));
  };
}
function Ib(l, u, i) {
  var s,
    o = i + "",
    f;
  return function () {
    var d = this.getAttributeNS(l.space, l.local);
    return d === o ? null : d === s ? f : (f = u((s = d), i));
  };
}
function Pb(l, u, i) {
  var s, o, f;
  return function () {
    var d,
      g = i(this),
      y;
    return g == null
      ? void this.removeAttribute(l)
      : ((d = this.getAttribute(l)),
        (y = g + ""),
        d === y
          ? null
          : d === s && y === o
            ? f
            : ((o = y), (f = u((s = d), g))));
  };
}
function t2(l, u, i) {
  var s, o, f;
  return function () {
    var d,
      g = i(this),
      y;
    return g == null
      ? void this.removeAttributeNS(l.space, l.local)
      : ((d = this.getAttributeNS(l.space, l.local)),
        (y = g + ""),
        d === y
          ? null
          : d === s && y === o
            ? f
            : ((o = y), (f = u((s = d), g))));
  };
}
function e2(l, u) {
  var i = cs(l),
    s = i === "transform" ? Rb : $g;
  return this.attrTween(
    l,
    typeof u == "function"
      ? (i.local ? t2 : Pb)(i, s, yf(this, "attr." + l, u))
      : u == null
        ? (i.local ? Wb : Jb)(i)
        : (i.local ? Ib : Fb)(i, s, u),
  );
}
function n2(l, u) {
  return function (i) {
    this.setAttribute(l, u.call(this, i));
  };
}
function l2(l, u) {
  return function (i) {
    this.setAttributeNS(l.space, l.local, u.call(this, i));
  };
}
function a2(l, u) {
  var i, s;
  function o() {
    var f = u.apply(this, arguments);
    return (f !== s && (i = (s = f) && l2(l, f)), i);
  }
  return ((o._value = u), o);
}
function u2(l, u) {
  var i, s;
  function o() {
    var f = u.apply(this, arguments);
    return (f !== s && (i = (s = f) && n2(l, f)), i);
  }
  return ((o._value = u), o);
}
function i2(l, u) {
  var i = "attr." + l;
  if (arguments.length < 2) return (i = this.tween(i)) && i._value;
  if (u == null) return this.tween(i, null);
  if (typeof u != "function") throw new Error();
  var s = cs(l);
  return this.tween(i, (s.local ? a2 : u2)(s, u));
}
function c2(l, u) {
  return function () {
    gf(this, l).delay = +u.apply(this, arguments);
  };
}
function s2(l, u) {
  return (
    (u = +u),
    function () {
      gf(this, l).delay = u;
    }
  );
}
function r2(l) {
  var u = this._id;
  return arguments.length
    ? this.each((typeof l == "function" ? c2 : s2)(u, l))
    : on(this.node(), u).delay;
}
function o2(l, u) {
  return function () {
    bn(this, l).duration = +u.apply(this, arguments);
  };
}
function f2(l, u) {
  return (
    (u = +u),
    function () {
      bn(this, l).duration = u;
    }
  );
}
function d2(l) {
  var u = this._id;
  return arguments.length
    ? this.each((typeof l == "function" ? o2 : f2)(u, l))
    : on(this.node(), u).duration;
}
function h2(l, u) {
  if (typeof u != "function") throw new Error();
  return function () {
    bn(this, l).ease = u;
  };
}
function m2(l) {
  var u = this._id;
  return arguments.length ? this.each(h2(u, l)) : on(this.node(), u).ease;
}
function g2(l, u) {
  return function () {
    var i = u.apply(this, arguments);
    if (typeof i != "function") throw new Error();
    bn(this, l).ease = i;
  };
}
function y2(l) {
  if (typeof l != "function") throw new Error();
  return this.each(g2(this._id, l));
}
function p2(l) {
  typeof l != "function" && (l = Ag(l));
  for (var u = this._groups, i = u.length, s = new Array(i), o = 0; o < i; ++o)
    for (var f = u[o], d = f.length, g = (s[o] = []), y, m = 0; m < d; ++m)
      (y = f[m]) && l.call(y, y.__data__, m, f) && g.push(y);
  return new kn(s, this._parents, this._name, this._id);
}
function v2(l) {
  if (l._id !== this._id) throw new Error();
  for (
    var u = this._groups,
      i = l._groups,
      s = u.length,
      o = i.length,
      f = Math.min(s, o),
      d = new Array(s),
      g = 0;
    g < f;
    ++g
  )
    for (
      var y = u[g], m = i[g], p = y.length, x = (d[g] = new Array(p)), _, E = 0;
      E < p;
      ++E
    )
      (_ = y[E] || m[E]) && (x[E] = _);
  for (; g < s; ++g) d[g] = u[g];
  return new kn(d, this._parents, this._name, this._id);
}
function x2(l) {
  return (l + "")
    .trim()
    .split(/^|\s+/)
    .every(function (u) {
      var i = u.indexOf(".");
      return (i >= 0 && (u = u.slice(0, i)), !u || u === "start");
    });
}
function b2(l, u, i) {
  var s,
    o,
    f = x2(u) ? gf : bn;
  return function () {
    var d = f(this, l),
      g = d.on;
    (g !== s && (o = (s = g).copy()).on(u, i), (d.on = o));
  };
}
function S2(l, u) {
  var i = this._id;
  return arguments.length < 2
    ? on(this.node(), i).on.on(l)
    : this.each(b2(i, l, u));
}
function E2(l) {
  return function () {
    var u = this.parentNode;
    for (var i in this.__transition) if (+i !== l) return;
    u && u.removeChild(this);
  };
}
function _2() {
  return this.on("end.remove", E2(this._id));
}
function w2(l) {
  var u = this._name,
    i = this._id;
  typeof l != "function" && (l = ff(l));
  for (var s = this._groups, o = s.length, f = new Array(o), d = 0; d < o; ++d)
    for (
      var g = s[d], y = g.length, m = (f[d] = new Array(y)), p, x, _ = 0;
      _ < y;
      ++_
    )
      (p = g[_]) &&
        (x = l.call(p, p.__data__, _, g)) &&
        ("__data__" in p && (x.__data__ = p.__data__),
        (m[_] = x),
        rs(m[_], u, i, _, m, on(p, i)));
  return new kn(f, this._parents, u, i);
}
function N2(l) {
  var u = this._name,
    i = this._id;
  typeof l != "function" && (l = Ng(l));
  for (var s = this._groups, o = s.length, f = [], d = [], g = 0; g < o; ++g)
    for (var y = s[g], m = y.length, p, x = 0; x < m; ++x)
      if ((p = y[x])) {
        for (
          var _ = l.call(p, p.__data__, x, y),
            E,
            D = on(p, i),
            z = 0,
            B = _.length;
          z < B;
          ++z
        )
          (E = _[z]) && rs(E, u, i, z, _, D);
        (f.push(_), d.push(p));
      }
  return new kn(f, d, u, i);
}
var A2 = di.prototype.constructor;
function T2() {
  return new A2(this._groups, this._parents);
}
function z2(l, u) {
  var i, s, o;
  return function () {
    var f = $a(this, l),
      d = (this.style.removeProperty(l), $a(this, l));
    return f === d ? null : f === i && d === s ? o : (o = u((i = f), (s = d)));
  };
}
function Jg(l) {
  return function () {
    this.style.removeProperty(l);
  };
}
function M2(l, u, i) {
  var s,
    o = i + "",
    f;
  return function () {
    var d = $a(this, l);
    return d === o ? null : d === s ? f : (f = u((s = d), i));
  };
}
function C2(l, u, i) {
  var s, o, f;
  return function () {
    var d = $a(this, l),
      g = i(this),
      y = g + "";
    return (
      g == null && (y = g = (this.style.removeProperty(l), $a(this, l))),
      d === y ? null : d === s && y === o ? f : ((o = y), (f = u((s = d), g)))
    );
  };
}
function D2(l, u) {
  var i,
    s,
    o,
    f = "style." + u,
    d = "end." + f,
    g;
  return function () {
    var y = bn(this, l),
      m = y.on,
      p = y.value[f] == null ? g || (g = Jg(u)) : void 0;
    ((m !== i || o !== p) && (s = (i = m).copy()).on(d, (o = p)), (y.on = s));
  };
}
function O2(l, u, i) {
  var s = (l += "") == "transform" ? Ob : $g;
  return u == null
    ? this.styleTween(l, z2(l, s)).on("end.style." + l, Jg(l))
    : typeof u == "function"
      ? this.styleTween(l, C2(l, s, yf(this, "style." + l, u))).each(
          D2(this._id, l),
        )
      : this.styleTween(l, M2(l, s, u), i).on("end.style." + l, null);
}
function R2(l, u, i) {
  return function (s) {
    this.style.setProperty(l, u.call(this, s), i);
  };
}
function H2(l, u, i) {
  var s, o;
  function f() {
    var d = u.apply(this, arguments);
    return (d !== o && (s = (o = d) && R2(l, d, i)), s);
  }
  return ((f._value = u), f);
}
function j2(l, u, i) {
  var s = "style." + (l += "");
  if (arguments.length < 2) return (s = this.tween(s)) && s._value;
  if (u == null) return this.tween(s, null);
  if (typeof u != "function") throw new Error();
  return this.tween(s, H2(l, u, i ?? ""));
}
function U2(l) {
  return function () {
    this.textContent = l;
  };
}
function B2(l) {
  return function () {
    var u = l(this);
    this.textContent = u ?? "";
  };
}
function Y2(l) {
  return this.tween(
    "text",
    typeof l == "function"
      ? B2(yf(this, "text", l))
      : U2(l == null ? "" : l + ""),
  );
}
function q2(l) {
  return function (u) {
    this.textContent = l.call(this, u);
  };
}
function X2(l) {
  var u, i;
  function s() {
    var o = l.apply(this, arguments);
    return (o !== i && (u = (i = o) && q2(o)), u);
  }
  return ((s._value = l), s);
}
function V2(l) {
  var u = "text";
  if (arguments.length < 1) return (u = this.tween(u)) && u._value;
  if (l == null) return this.tween(u, null);
  if (typeof l != "function") throw new Error();
  return this.tween(u, X2(l));
}
function L2() {
  for (
    var l = this._name,
      u = this._id,
      i = Wg(),
      s = this._groups,
      o = s.length,
      f = 0;
    f < o;
    ++f
  )
    for (var d = s[f], g = d.length, y, m = 0; m < g; ++m)
      if ((y = d[m])) {
        var p = on(y, u);
        rs(y, l, i, m, d, {
          time: p.time + p.delay + p.duration,
          delay: 0,
          duration: p.duration,
          ease: p.ease,
        });
      }
  return new kn(s, this._parents, l, i);
}
function G2() {
  var l,
    u,
    i = this,
    s = i._id,
    o = i.size();
  return new Promise(function (f, d) {
    var g = { value: d },
      y = {
        value: function () {
          --o === 0 && f();
        },
      };
    (i.each(function () {
      var m = bn(this, s),
        p = m.on;
      (p !== l &&
        ((u = (l = p).copy()),
        u._.cancel.push(g),
        u._.interrupt.push(g),
        u._.end.push(y)),
        (m.on = u));
    }),
      o === 0 && f());
  });
}
var Z2 = 0;
function kn(l, u, i, s) {
  ((this._groups = l), (this._parents = u), (this._name = i), (this._id = s));
}
function Wg() {
  return ++Z2;
}
var Gn = di.prototype;
kn.prototype = {
  constructor: kn,
  select: w2,
  selectAll: N2,
  selectChild: Gn.selectChild,
  selectChildren: Gn.selectChildren,
  filter: p2,
  merge: v2,
  selection: T2,
  transition: L2,
  call: Gn.call,
  nodes: Gn.nodes,
  node: Gn.node,
  size: Gn.size,
  empty: Gn.empty,
  each: Gn.each,
  on: S2,
  attr: e2,
  attrTween: i2,
  style: O2,
  styleTween: j2,
  text: Y2,
  textTween: V2,
  remove: _2,
  tween: $b,
  delay: r2,
  duration: d2,
  ease: m2,
  easeVarying: y2,
  end: G2,
  [Symbol.iterator]: Gn[Symbol.iterator],
};
function Q2(l) {
  return ((l *= 2) <= 1 ? l * l * l : (l -= 2) * l * l + 2) / 2;
}
var k2 = { time: null, delay: 0, duration: 250, ease: Q2 };
function K2(l, u) {
  for (var i; !(i = l.__transition) || !(i = i[u]); )
    if (!(l = l.parentNode)) throw new Error(`transition ${u} not found`);
  return i;
}
function $2(l) {
  var u, i;
  l instanceof kn
    ? ((u = l._id), (l = l._name))
    : ((u = Wg()), ((i = k2).time = mf()), (l = l == null ? null : l + ""));
  for (var s = this._groups, o = s.length, f = 0; f < o; ++f)
    for (var d = s[f], g = d.length, y, m = 0; m < g; ++m)
      (y = d[m]) && rs(y, l, u, m, d, i || K2(y, u));
  return new kn(s, this._parents, l, u);
}
di.prototype.interrupt = Qb;
di.prototype.transition = $2;
const Lc = (l) => () => l;
function J2(l, { sourceEvent: u, target: i, transform: s, dispatch: o }) {
  Object.defineProperties(this, {
    type: { value: l, enumerable: !0, configurable: !0 },
    sourceEvent: { value: u, enumerable: !0, configurable: !0 },
    target: { value: i, enumerable: !0, configurable: !0 },
    transform: { value: s, enumerable: !0, configurable: !0 },
    _: { value: o },
  });
}
function Zn(l, u, i) {
  ((this.k = l), (this.x = u), (this.y = i));
}
Zn.prototype = {
  constructor: Zn,
  scale: function (l) {
    return l === 1 ? this : new Zn(this.k * l, this.x, this.y);
  },
  translate: function (l, u) {
    return (l === 0) & (u === 0)
      ? this
      : new Zn(this.k, this.x + this.k * l, this.y + this.k * u);
  },
  apply: function (l) {
    return [l[0] * this.k + this.x, l[1] * this.k + this.y];
  },
  applyX: function (l) {
    return l * this.k + this.x;
  },
  applyY: function (l) {
    return l * this.k + this.y;
  },
  invert: function (l) {
    return [(l[0] - this.x) / this.k, (l[1] - this.y) / this.k];
  },
  invertX: function (l) {
    return (l - this.x) / this.k;
  },
  invertY: function (l) {
    return (l - this.y) / this.k;
  },
  rescaleX: function (l) {
    return l.copy().domain(l.range().map(this.invertX, this).map(l.invert, l));
  },
  rescaleY: function (l) {
    return l.copy().domain(l.range().map(this.invertY, this).map(l.invert, l));
  },
  toString: function () {
    return "translate(" + this.x + "," + this.y + ") scale(" + this.k + ")";
  },
};
var Qn = new Zn(1, 0, 0);
Zn.prototype;
function Xo(l) {
  l.stopImmediatePropagation();
}
function Wu(l) {
  (l.preventDefault(), l.stopImmediatePropagation());
}
function W2(l) {
  return (!l.ctrlKey || l.type === "wheel") && !l.button;
}
function F2() {
  var l = this;
  return l instanceof SVGElement
    ? ((l = l.ownerSVGElement || l),
      l.hasAttribute("viewBox")
        ? ((l = l.viewBox.baseVal),
          [
            [l.x, l.y],
            [l.x + l.width, l.y + l.height],
          ])
        : [
            [0, 0],
            [l.width.baseVal.value, l.height.baseVal.value],
          ])
    : [
        [0, 0],
        [l.clientWidth, l.clientHeight],
      ];
}
function K0() {
  return this.__zoom || Qn;
}
function I2(l) {
  return (
    -l.deltaY *
    (l.deltaMode === 1 ? 0.05 : l.deltaMode ? 1 : 0.002) *
    (l.ctrlKey ? 10 : 1)
  );
}
function P2() {
  return navigator.maxTouchPoints || "ontouchstart" in this;
}
function tS(l, u, i) {
  var s = l.invertX(u[0][0]) - i[0][0],
    o = l.invertX(u[1][0]) - i[1][0],
    f = l.invertY(u[0][1]) - i[0][1],
    d = l.invertY(u[1][1]) - i[1][1];
  return l.translate(
    o > s ? (s + o) / 2 : Math.min(0, s) || Math.max(0, o),
    d > f ? (f + d) / 2 : Math.min(0, f) || Math.max(0, d),
  );
}
function Fg() {
  var l = W2,
    u = F2,
    i = tS,
    s = I2,
    o = P2,
    f = [0, 1 / 0],
    d = [
      [-1 / 0, -1 / 0],
      [1 / 0, 1 / 0],
    ],
    g = 250,
    y = Bb,
    m = is("start", "zoom", "end"),
    p,
    x,
    _,
    E = 500,
    D = 150,
    z = 0,
    B = 10;
  function N(w) {
    w.property("__zoom", K0)
      .on("wheel.zoom", ut, { passive: !1 })
      .on("mousedown.zoom", et)
      .on("dblclick.zoom", ct)
      .filter(o)
      .on("touchstart.zoom", at)
      .on("touchmove.zoom", st)
      .on("touchend.zoom touchcancel.zoom", rt)
      .style("-webkit-tap-highlight-color", "rgba(0,0,0,0)");
  }
  ((N.transform = function (w, L, S, R) {
    var Z = w.selection ? w.selection() : w;
    (Z.property("__zoom", K0),
      w !== Z
        ? W(w, L, S, R)
        : Z.interrupt().each(function () {
            I(this, arguments)
              .event(R)
              .start()
              .zoom(null, typeof L == "function" ? L.apply(this, arguments) : L)
              .end();
          }));
  }),
    (N.scaleBy = function (w, L, S, R) {
      N.scaleTo(
        w,
        function () {
          var Z = this.__zoom.k,
            O = typeof L == "function" ? L.apply(this, arguments) : L;
          return Z * O;
        },
        S,
        R,
      );
    }),
    (N.scaleTo = function (w, L, S, R) {
      N.transform(
        w,
        function () {
          var Z = u.apply(this, arguments),
            O = this.__zoom,
            k =
              S == null
                ? j(Z)
                : typeof S == "function"
                  ? S.apply(this, arguments)
                  : S,
            b = O.invert(k),
            M = typeof L == "function" ? L.apply(this, arguments) : L;
          return i(G(Y(O, M), k, b), Z, d);
        },
        S,
        R,
      );
    }),
    (N.translateBy = function (w, L, S, R) {
      N.transform(
        w,
        function () {
          return i(
            this.__zoom.translate(
              typeof L == "function" ? L.apply(this, arguments) : L,
              typeof S == "function" ? S.apply(this, arguments) : S,
            ),
            u.apply(this, arguments),
            d,
          );
        },
        null,
        R,
      );
    }),
    (N.translateTo = function (w, L, S, R, Z) {
      N.transform(
        w,
        function () {
          var O = u.apply(this, arguments),
            k = this.__zoom,
            b =
              R == null
                ? j(O)
                : typeof R == "function"
                  ? R.apply(this, arguments)
                  : R;
          return i(
            Qn.translate(b[0], b[1])
              .scale(k.k)
              .translate(
                typeof L == "function" ? -L.apply(this, arguments) : -L,
                typeof S == "function" ? -S.apply(this, arguments) : -S,
              ),
            O,
            d,
          );
        },
        R,
        Z,
      );
    }));
  function Y(w, L) {
    return (
      (L = Math.max(f[0], Math.min(f[1], L))),
      L === w.k ? w : new Zn(L, w.x, w.y)
    );
  }
  function G(w, L, S) {
    var R = L[0] - S[0] * w.k,
      Z = L[1] - S[1] * w.k;
    return R === w.x && Z === w.y ? w : new Zn(w.k, R, Z);
  }
  function j(w) {
    return [(+w[0][0] + +w[1][0]) / 2, (+w[0][1] + +w[1][1]) / 2];
  }
  function W(w, L, S, R) {
    w.on("start.zoom", function () {
      I(this, arguments).event(R).start();
    })
      .on("interrupt.zoom end.zoom", function () {
        I(this, arguments).event(R).end();
      })
      .tween("zoom", function () {
        var Z = this,
          O = arguments,
          k = I(Z, O).event(R),
          b = u.apply(Z, O),
          M = S == null ? j(b) : typeof S == "function" ? S.apply(Z, O) : S,
          $ = Math.max(b[1][0] - b[0][0], b[1][1] - b[0][1]),
          F = Z.__zoom,
          it = typeof L == "function" ? L.apply(Z, O) : L,
          ot = y(F.invert(M).concat($ / F.k), it.invert(M).concat($ / it.k));
        return function (dt) {
          if (dt === 1) dt = it;
          else {
            var tt = ot(dt),
              ft = $ / tt[2];
            dt = new Zn(ft, M[0] - tt[0] * ft, M[1] - tt[1] * ft);
          }
          k.zoom(null, dt);
        };
      });
  }
  function I(w, L, S) {
    return (!S && w.__zooming) || new Q(w, L);
  }
  function Q(w, L) {
    ((this.that = w),
      (this.args = L),
      (this.active = 0),
      (this.sourceEvent = null),
      (this.extent = u.apply(w, L)),
      (this.taps = 0));
  }
  Q.prototype = {
    event: function (w) {
      return (w && (this.sourceEvent = w), this);
    },
    start: function () {
      return (
        ++this.active === 1 &&
          ((this.that.__zooming = this), this.emit("start")),
        this
      );
    },
    zoom: function (w, L) {
      return (
        this.mouse &&
          w !== "mouse" &&
          (this.mouse[1] = L.invert(this.mouse[0])),
        this.touch0 &&
          w !== "touch" &&
          (this.touch0[1] = L.invert(this.touch0[0])),
        this.touch1 &&
          w !== "touch" &&
          (this.touch1[1] = L.invert(this.touch1[0])),
        (this.that.__zoom = L),
        this.emit("zoom"),
        this
      );
    },
    end: function () {
      return (
        --this.active === 0 && (delete this.that.__zooming, this.emit("end")),
        this
      );
    },
    emit: function (w) {
      var L = tn(this.that).datum();
      m.call(
        w,
        this.that,
        new J2(w, {
          sourceEvent: this.sourceEvent,
          target: N,
          transform: this.that.__zoom,
          dispatch: m,
        }),
        L,
      );
    },
  };
  function ut(w, ...L) {
    if (!l.apply(this, arguments)) return;
    var S = I(this, L).event(w),
      R = this.__zoom,
      Z = Math.max(
        f[0],
        Math.min(f[1], R.k * Math.pow(2, s.apply(this, arguments))),
      ),
      O = cn(w);
    if (S.wheel)
      ((S.mouse[0][0] !== O[0] || S.mouse[0][1] !== O[1]) &&
        (S.mouse[1] = R.invert((S.mouse[0] = O))),
        clearTimeout(S.wheel));
    else {
      if (R.k === Z) return;
      ((S.mouse = [O, R.invert(O)]), Wc(this), S.start());
    }
    (Wu(w),
      (S.wheel = setTimeout(k, D)),
      S.zoom("mouse", i(G(Y(R, Z), S.mouse[0], S.mouse[1]), S.extent, d)));
    function k() {
      ((S.wheel = null), S.end());
    }
  }
  function et(w, ...L) {
    if (_ || !l.apply(this, arguments)) return;
    var S = w.currentTarget,
      R = I(this, L, !0).event(w),
      Z = tn(w.view).on("mousemove.zoom", M, !0).on("mouseup.zoom", $, !0),
      O = cn(w, S),
      k = w.clientX,
      b = w.clientY;
    (Ug(w.view),
      Xo(w),
      (R.mouse = [O, this.__zoom.invert(O)]),
      Wc(this),
      R.start());
    function M(F) {
      if ((Wu(F), !R.moved)) {
        var it = F.clientX - k,
          ot = F.clientY - b;
        R.moved = it * it + ot * ot > z;
      }
      R.event(F).zoom(
        "mouse",
        i(G(R.that.__zoom, (R.mouse[0] = cn(F, S)), R.mouse[1]), R.extent, d),
      );
    }
    function $(F) {
      (Z.on("mousemove.zoom mouseup.zoom", null),
        Bg(F.view, R.moved),
        Wu(F),
        R.event(F).end());
    }
  }
  function ct(w, ...L) {
    if (l.apply(this, arguments)) {
      var S = this.__zoom,
        R = cn(w.changedTouches ? w.changedTouches[0] : w, this),
        Z = S.invert(R),
        O = S.k * (w.shiftKey ? 0.5 : 2),
        k = i(G(Y(S, O), R, Z), u.apply(this, L), d);
      (Wu(w),
        g > 0
          ? tn(this).transition().duration(g).call(W, k, R, w)
          : tn(this).call(N.transform, k, R, w));
    }
  }
  function at(w, ...L) {
    if (l.apply(this, arguments)) {
      var S = w.touches,
        R = S.length,
        Z = I(this, L, w.changedTouches.length === R).event(w),
        O,
        k,
        b,
        M;
      for (Xo(w), k = 0; k < R; ++k)
        ((b = S[k]),
          (M = cn(b, this)),
          (M = [M, this.__zoom.invert(M), b.identifier]),
          Z.touch0
            ? !Z.touch1 &&
              Z.touch0[2] !== M[2] &&
              ((Z.touch1 = M), (Z.taps = 0))
            : ((Z.touch0 = M), (O = !0), (Z.taps = 1 + !!p)));
      (p && (p = clearTimeout(p)),
        O &&
          (Z.taps < 2 &&
            ((x = M[0]),
            (p = setTimeout(function () {
              p = null;
            }, E))),
          Wc(this),
          Z.start()));
    }
  }
  function st(w, ...L) {
    if (this.__zooming) {
      var S = I(this, L).event(w),
        R = w.changedTouches,
        Z = R.length,
        O,
        k,
        b,
        M;
      for (Wu(w), O = 0; O < Z; ++O)
        ((k = R[O]),
          (b = cn(k, this)),
          S.touch0 && S.touch0[2] === k.identifier
            ? (S.touch0[0] = b)
            : S.touch1 && S.touch1[2] === k.identifier && (S.touch1[0] = b));
      if (((k = S.that.__zoom), S.touch1)) {
        var $ = S.touch0[0],
          F = S.touch0[1],
          it = S.touch1[0],
          ot = S.touch1[1],
          dt = (dt = it[0] - $[0]) * dt + (dt = it[1] - $[1]) * dt,
          tt = (tt = ot[0] - F[0]) * tt + (tt = ot[1] - F[1]) * tt;
        ((k = Y(k, Math.sqrt(dt / tt))),
          (b = [($[0] + it[0]) / 2, ($[1] + it[1]) / 2]),
          (M = [(F[0] + ot[0]) / 2, (F[1] + ot[1]) / 2]));
      } else if (S.touch0) ((b = S.touch0[0]), (M = S.touch0[1]));
      else return;
      S.zoom("touch", i(G(k, b, M), S.extent, d));
    }
  }
  function rt(w, ...L) {
    if (this.__zooming) {
      var S = I(this, L).event(w),
        R = w.changedTouches,
        Z = R.length,
        O,
        k;
      for (
        Xo(w),
          _ && clearTimeout(_),
          _ = setTimeout(function () {
            _ = null;
          }, E),
          O = 0;
        O < Z;
        ++O
      )
        ((k = R[O]),
          S.touch0 && S.touch0[2] === k.identifier
            ? delete S.touch0
            : S.touch1 && S.touch1[2] === k.identifier && delete S.touch1);
      if (
        (S.touch1 && !S.touch0 && ((S.touch0 = S.touch1), delete S.touch1),
        S.touch0)
      )
        S.touch0[1] = this.__zoom.invert(S.touch0[0]);
      else if (
        (S.end(),
        S.taps === 2 &&
          ((k = cn(k, this)), Math.hypot(x[0] - k[0], x[1] - k[1]) < B))
      ) {
        var b = tn(this).on("dblclick.zoom");
        b && b.apply(this, arguments);
      }
    }
  }
  return (
    (N.wheelDelta = function (w) {
      return arguments.length
        ? ((s = typeof w == "function" ? w : Lc(+w)), N)
        : s;
    }),
    (N.filter = function (w) {
      return arguments.length
        ? ((l = typeof w == "function" ? w : Lc(!!w)), N)
        : l;
    }),
    (N.touchable = function (w) {
      return arguments.length
        ? ((o = typeof w == "function" ? w : Lc(!!w)), N)
        : o;
    }),
    (N.extent = function (w) {
      return arguments.length
        ? ((u =
            typeof w == "function"
              ? w
              : Lc([
                  [+w[0][0], +w[0][1]],
                  [+w[1][0], +w[1][1]],
                ])),
          N)
        : u;
    }),
    (N.scaleExtent = function (w) {
      return arguments.length
        ? ((f[0] = +w[0]), (f[1] = +w[1]), N)
        : [f[0], f[1]];
    }),
    (N.translateExtent = function (w) {
      return arguments.length
        ? ((d[0][0] = +w[0][0]),
          (d[1][0] = +w[1][0]),
          (d[0][1] = +w[0][1]),
          (d[1][1] = +w[1][1]),
          N)
        : [
            [d[0][0], d[0][1]],
            [d[1][0], d[1][1]],
          ];
    }),
    (N.constrain = function (w) {
      return arguments.length ? ((i = w), N) : i;
    }),
    (N.duration = function (w) {
      return arguments.length ? ((g = +w), N) : g;
    }),
    (N.interpolate = function (w) {
      return arguments.length ? ((y = w), N) : y;
    }),
    (N.on = function () {
      var w = m.on.apply(m, arguments);
      return w === m ? N : w;
    }),
    (N.clickDistance = function (w) {
      return arguments.length ? ((z = (w = +w) * w), N) : Math.sqrt(z);
    }),
    (N.tapDistance = function (w) {
      return arguments.length ? ((B = +w), N) : B;
    }),
    N
  );
}
bg();
const os = H.createContext(null),
  eS = os.Provider,
  Kn = {
    error001: () =>
      "[React Flow]: Seems like you have not used zustand provider as an ancestor. Help: https://reactflow.dev/error#001",
    error002: () =>
      "It looks like you've created a new nodeTypes or edgeTypes object. If this wasn't on purpose please define the nodeTypes/edgeTypes outside of the component or memoize them.",
    error003: (l) =>
      `Node type "${l}" not found. Using fallback type "default".`,
    error004: () =>
      "The React Flow parent container needs a width and a height to render the graph.",
    error005: () => "Only child nodes can use a parent extent.",
    error006: () => "Can't create edge. An edge needs a source and a target.",
    error007: (l) => `The old edge with id=${l} does not exist.`,
    error009: (l) => `Marker type "${l}" doesn't exist.`,
    error008: (l, u) =>
      `Couldn't create edge for ${l ? "target" : "source"} handle id: "${l ? u.targetHandle : u.sourceHandle}", edge id: ${u.id}.`,
    error010: () =>
      "Handle: No node id found. Make sure to only use a Handle inside a custom Node.",
    error011: (l) =>
      `Edge type "${l}" not found. Using fallback type "default".`,
    error012: (l) =>
      `Node with id "${l}" does not exist, it may have been removed. This can happen when a node is deleted before the "onNodeClick" handler is called.`,
  },
  Ig = Kn.error001();
function Gt(l, u) {
  const i = H.useContext(os);
  if (i === null) throw new Error(Ig);
  return _g(i, l, u);
}
const fe = () => {
    const l = H.useContext(os);
    if (l === null) throw new Error(Ig);
    return H.useMemo(
      () => ({
        getState: l.getState,
        setState: l.setState,
        subscribe: l.subscribe,
        destroy: l.destroy,
      }),
      [l],
    );
  },
  nS = (l) => (l.userSelectionActive ? "none" : "all");
function pf({ position: l, children: u, className: i, style: s, ...o }) {
  const f = Gt(nS),
    d = `${l}`.split("-");
  return J.createElement(
    "div",
    {
      className: be(["react-flow__panel", i, ...d]),
      style: { ...s, pointerEvents: f },
      ...o,
    },
    u,
  );
}
function lS({ proOptions: l, position: u = "bottom-right" }) {
  return l != null && l.hideAttribution
    ? null
    : J.createElement(
        pf,
        {
          position: u,
          className: "react-flow__attribution",
          "data-message":
            "Please only hide this attribution when you are subscribed to React Flow Pro: https://reactflow.dev/pro",
        },
        J.createElement(
          "a",
          {
            href: "https://reactflow.dev",
            target: "_blank",
            rel: "noopener noreferrer",
            "aria-label": "React Flow attribution",
          },
          "React Flow",
        ),
      );
}
const aS = ({
  x: l,
  y: u,
  label: i,
  labelStyle: s = {},
  labelShowBg: o = !0,
  labelBgStyle: f = {},
  labelBgPadding: d = [2, 4],
  labelBgBorderRadius: g = 2,
  children: y,
  className: m,
  ...p
}) => {
  const x = H.useRef(null),
    [_, E] = H.useState({ x: 0, y: 0, width: 0, height: 0 }),
    D = be(["react-flow__edge-textwrapper", m]);
  return (
    H.useEffect(() => {
      if (x.current) {
        const z = x.current.getBBox();
        E({ x: z.x, y: z.y, width: z.width, height: z.height });
      }
    }, [i]),
    typeof i > "u" || !i
      ? null
      : J.createElement(
          "g",
          {
            transform: `translate(${l - _.width / 2} ${u - _.height / 2})`,
            className: D,
            visibility: _.width ? "visible" : "hidden",
            ...p,
          },
          o &&
            J.createElement("rect", {
              width: _.width + 2 * d[0],
              x: -d[0],
              y: -d[1],
              height: _.height + 2 * d[1],
              className: "react-flow__edge-textbg",
              style: f,
              rx: g,
              ry: g,
            }),
          J.createElement(
            "text",
            {
              className: "react-flow__edge-text",
              y: _.height / 2,
              dy: "0.3em",
              ref: x,
              style: s,
            },
            i,
          ),
          y,
        )
  );
};
var uS = H.memo(aS);
const vf = (l) => ({ width: l.offsetWidth, height: l.offsetHeight }),
  Wa = (l, u = 0, i = 1) => Math.min(Math.max(l, u), i),
  xf = (l = { x: 0, y: 0 }, u) => ({
    x: Wa(l.x, u[0][0], u[1][0]),
    y: Wa(l.y, u[0][1], u[1][1]),
  }),
  $0 = (l, u, i) =>
    l < u
      ? Wa(Math.abs(l - u), 1, 50) / 50
      : l > i
        ? -Wa(Math.abs(l - i), 1, 50) / 50
        : 0,
  Pg = (l, u) => {
    const i = $0(l.x, 35, u.width - 35) * 20,
      s = $0(l.y, 35, u.height - 35) * 20;
    return [i, s];
  },
  ty = (l) => {
    var u;
    return (
      ((u = l.getRootNode) == null ? void 0 : u.call(l)) ||
      (window == null ? void 0 : window.document)
    );
  },
  ey = (l, u) => ({
    x: Math.min(l.x, u.x),
    y: Math.min(l.y, u.y),
    x2: Math.max(l.x2, u.x2),
    y2: Math.max(l.y2, u.y2),
  }),
  si = ({ x: l, y: u, width: i, height: s }) => ({
    x: l,
    y: u,
    x2: l + i,
    y2: u + s,
  }),
  ny = ({ x: l, y: u, x2: i, y2: s }) => ({
    x: l,
    y: u,
    width: i - l,
    height: s - u,
  }),
  J0 = (l) => ({
    ...(l.positionAbsolute || { x: 0, y: 0 }),
    width: l.width || 0,
    height: l.height || 0,
  }),
  iS = (l, u) => ny(ey(si(l), si(u))),
  ef = (l, u) => {
    const i = Math.max(
        0,
        Math.min(l.x + l.width, u.x + u.width) - Math.max(l.x, u.x),
      ),
      s = Math.max(
        0,
        Math.min(l.y + l.height, u.y + u.height) - Math.max(l.y, u.y),
      );
    return Math.ceil(i * s);
  },
  cS = (l) => en(l.width) && en(l.height) && en(l.x) && en(l.y),
  en = (l) => !isNaN(l) && isFinite(l),
  Ft = Symbol.for("internals"),
  ly = ["Enter", " ", "Escape"],
  sS = (l, u) => {},
  rS = (l) => "nativeEvent" in l;
function nf(l) {
  var o, f;
  const u = rS(l) ? l.nativeEvent : l,
    i =
      ((f = (o = u.composedPath) == null ? void 0 : o.call(u)) == null
        ? void 0
        : f[0]) || l.target;
  return (
    ["INPUT", "SELECT", "TEXTAREA"].includes(i == null ? void 0 : i.nodeName) ||
    (i == null ? void 0 : i.hasAttribute("contenteditable")) ||
    !!(i != null && i.closest(".nokey"))
  );
}
const ay = (l) => "clientX" in l,
  Cl = (l, u) => {
    var f, d;
    const i = ay(l),
      s = i ? l.clientX : (f = l.touches) == null ? void 0 : f[0].clientX,
      o = i ? l.clientY : (d = l.touches) == null ? void 0 : d[0].clientY;
    return {
      x: s - ((u == null ? void 0 : u.left) ?? 0),
      y: o - ((u == null ? void 0 : u.top) ?? 0),
    };
  },
  ls = () => {
    var l;
    return (
      typeof navigator < "u" &&
      ((l = navigator == null ? void 0 : navigator.userAgent) == null
        ? void 0
        : l.indexOf("Mac")) >= 0
    );
  },
  mi = ({
    id: l,
    path: u,
    labelX: i,
    labelY: s,
    label: o,
    labelStyle: f,
    labelShowBg: d,
    labelBgStyle: g,
    labelBgPadding: y,
    labelBgBorderRadius: m,
    style: p,
    markerEnd: x,
    markerStart: _,
    interactionWidth: E = 20,
  }) =>
    J.createElement(
      J.Fragment,
      null,
      J.createElement("path", {
        id: l,
        style: p,
        d: u,
        fill: "none",
        className: "react-flow__edge-path",
        markerEnd: x,
        markerStart: _,
      }),
      E &&
        J.createElement("path", {
          d: u,
          fill: "none",
          strokeOpacity: 0,
          strokeWidth: E,
          className: "react-flow__edge-interaction",
        }),
      o && en(i) && en(s)
        ? J.createElement(uS, {
            x: i,
            y: s,
            label: o,
            labelStyle: f,
            labelShowBg: d,
            labelBgStyle: g,
            labelBgPadding: y,
            labelBgBorderRadius: m,
          })
        : null,
    );
mi.displayName = "BaseEdge";
function Fu(l, u, i) {
  return i === void 0
    ? i
    : (s) => {
        const o = u().edges.find((f) => f.id === l);
        o && i(s, { ...o });
      };
}
function uy({ sourceX: l, sourceY: u, targetX: i, targetY: s }) {
  const o = Math.abs(i - l) / 2,
    f = i < l ? i + o : i - o,
    d = Math.abs(s - u) / 2,
    g = s < u ? s + d : s - d;
  return [f, g, o, d];
}
function iy({
  sourceX: l,
  sourceY: u,
  targetX: i,
  targetY: s,
  sourceControlX: o,
  sourceControlY: f,
  targetControlX: d,
  targetControlY: g,
}) {
  const y = l * 0.125 + o * 0.375 + d * 0.375 + i * 0.125,
    m = u * 0.125 + f * 0.375 + g * 0.375 + s * 0.125,
    p = Math.abs(y - l),
    x = Math.abs(m - u);
  return [y, m, p, x];
}
var ua;
(function (l) {
  ((l.Strict = "strict"), (l.Loose = "loose"));
})(ua || (ua = {}));
var ea;
(function (l) {
  ((l.Free = "free"), (l.Vertical = "vertical"), (l.Horizontal = "horizontal"));
})(ea || (ea = {}));
var ri;
(function (l) {
  ((l.Partial = "partial"), (l.Full = "full"));
})(ri || (ri = {}));
var Ml;
(function (l) {
  ((l.Bezier = "default"),
    (l.Straight = "straight"),
    (l.Step = "step"),
    (l.SmoothStep = "smoothstep"),
    (l.SimpleBezier = "simplebezier"));
})(Ml || (Ml = {}));
var as;
(function (l) {
  ((l.Arrow = "arrow"), (l.ArrowClosed = "arrowclosed"));
})(as || (as = {}));
var pt;
(function (l) {
  ((l.Left = "left"),
    (l.Top = "top"),
    (l.Right = "right"),
    (l.Bottom = "bottom"));
})(pt || (pt = {}));
function W0({ pos: l, x1: u, y1: i, x2: s, y2: o }) {
  return l === pt.Left || l === pt.Right
    ? [0.5 * (u + s), i]
    : [u, 0.5 * (i + o)];
}
function cy({
  sourceX: l,
  sourceY: u,
  sourcePosition: i = pt.Bottom,
  targetX: s,
  targetY: o,
  targetPosition: f = pt.Top,
}) {
  const [d, g] = W0({ pos: i, x1: l, y1: u, x2: s, y2: o }),
    [y, m] = W0({ pos: f, x1: s, y1: o, x2: l, y2: u }),
    [p, x, _, E] = iy({
      sourceX: l,
      sourceY: u,
      targetX: s,
      targetY: o,
      sourceControlX: d,
      sourceControlY: g,
      targetControlX: y,
      targetControlY: m,
    });
  return [`M${l},${u} C${d},${g} ${y},${m} ${s},${o}`, p, x, _, E];
}
const bf = H.memo(
  ({
    sourceX: l,
    sourceY: u,
    targetX: i,
    targetY: s,
    sourcePosition: o = pt.Bottom,
    targetPosition: f = pt.Top,
    label: d,
    labelStyle: g,
    labelShowBg: y,
    labelBgStyle: m,
    labelBgPadding: p,
    labelBgBorderRadius: x,
    style: _,
    markerEnd: E,
    markerStart: D,
    interactionWidth: z,
  }) => {
    const [B, N, Y] = cy({
      sourceX: l,
      sourceY: u,
      sourcePosition: o,
      targetX: i,
      targetY: s,
      targetPosition: f,
    });
    return J.createElement(mi, {
      path: B,
      labelX: N,
      labelY: Y,
      label: d,
      labelStyle: g,
      labelShowBg: y,
      labelBgStyle: m,
      labelBgPadding: p,
      labelBgBorderRadius: x,
      style: _,
      markerEnd: E,
      markerStart: D,
      interactionWidth: z,
    });
  },
);
bf.displayName = "SimpleBezierEdge";
const F0 = {
    [pt.Left]: { x: -1, y: 0 },
    [pt.Right]: { x: 1, y: 0 },
    [pt.Top]: { x: 0, y: -1 },
    [pt.Bottom]: { x: 0, y: 1 },
  },
  oS = ({ source: l, sourcePosition: u = pt.Bottom, target: i }) =>
    u === pt.Left || u === pt.Right
      ? l.x < i.x
        ? { x: 1, y: 0 }
        : { x: -1, y: 0 }
      : l.y < i.y
        ? { x: 0, y: 1 }
        : { x: 0, y: -1 },
  I0 = (l, u) => Math.sqrt(Math.pow(u.x - l.x, 2) + Math.pow(u.y - l.y, 2));
function fS({
  source: l,
  sourcePosition: u = pt.Bottom,
  target: i,
  targetPosition: s = pt.Top,
  center: o,
  offset: f,
}) {
  const d = F0[u],
    g = F0[s],
    y = { x: l.x + d.x * f, y: l.y + d.y * f },
    m = { x: i.x + g.x * f, y: i.y + g.y * f },
    p = oS({ source: y, sourcePosition: u, target: m }),
    x = p.x !== 0 ? "x" : "y",
    _ = p[x];
  let E = [],
    D,
    z;
  const B = { x: 0, y: 0 },
    N = { x: 0, y: 0 },
    [Y, G, j, W] = uy({
      sourceX: l.x,
      sourceY: l.y,
      targetX: i.x,
      targetY: i.y,
    });
  if (d[x] * g[x] === -1) {
    ((D = o.x ?? Y), (z = o.y ?? G));
    const Q = [
        { x: D, y: y.y },
        { x: D, y: m.y },
      ],
      ut = [
        { x: y.x, y: z },
        { x: m.x, y: z },
      ];
    d[x] === _ ? (E = x === "x" ? Q : ut) : (E = x === "x" ? ut : Q);
  } else {
    const Q = [{ x: y.x, y: m.y }],
      ut = [{ x: m.x, y: y.y }];
    if (
      (x === "x" ? (E = d.x === _ ? ut : Q) : (E = d.y === _ ? Q : ut), u === s)
    ) {
      const rt = Math.abs(l[x] - i[x]);
      if (rt <= f) {
        const w = Math.min(f - 1, f - rt);
        d[x] === _
          ? (B[x] = (y[x] > l[x] ? -1 : 1) * w)
          : (N[x] = (m[x] > i[x] ? -1 : 1) * w);
      }
    }
    if (u !== s) {
      const rt = x === "x" ? "y" : "x",
        w = d[x] === g[rt],
        L = y[rt] > m[rt],
        S = y[rt] < m[rt];
      ((d[x] === 1 && ((!w && L) || (w && S))) ||
        (d[x] !== 1 && ((!w && S) || (w && L)))) &&
        (E = x === "x" ? Q : ut);
    }
    const et = { x: y.x + B.x, y: y.y + B.y },
      ct = { x: m.x + N.x, y: m.y + N.y },
      at = Math.max(Math.abs(et.x - E[0].x), Math.abs(ct.x - E[0].x)),
      st = Math.max(Math.abs(et.y - E[0].y), Math.abs(ct.y - E[0].y));
    at >= st
      ? ((D = (et.x + ct.x) / 2), (z = E[0].y))
      : ((D = E[0].x), (z = (et.y + ct.y) / 2));
  }
  return [
    [
      l,
      { x: y.x + B.x, y: y.y + B.y },
      ...E,
      { x: m.x + N.x, y: m.y + N.y },
      i,
    ],
    D,
    z,
    j,
    W,
  ];
}
function dS(l, u, i, s) {
  const o = Math.min(I0(l, u) / 2, I0(u, i) / 2, s),
    { x: f, y: d } = u;
  if ((l.x === f && f === i.x) || (l.y === d && d === i.y)) return `L${f} ${d}`;
  if (l.y === d) {
    const m = l.x < i.x ? -1 : 1,
      p = l.y < i.y ? 1 : -1;
    return `L ${f + o * m},${d}Q ${f},${d} ${f},${d + o * p}`;
  }
  const g = l.x < i.x ? 1 : -1,
    y = l.y < i.y ? -1 : 1;
  return `L ${f},${d + o * y}Q ${f},${d} ${f + o * g},${d}`;
}
function lf({
  sourceX: l,
  sourceY: u,
  sourcePosition: i = pt.Bottom,
  targetX: s,
  targetY: o,
  targetPosition: f = pt.Top,
  borderRadius: d = 5,
  centerX: g,
  centerY: y,
  offset: m = 20,
}) {
  const [p, x, _, E, D] = fS({
    source: { x: l, y: u },
    sourcePosition: i,
    target: { x: s, y: o },
    targetPosition: f,
    center: { x: g, y },
    offset: m,
  });
  return [
    p.reduce((B, N, Y) => {
      let G = "";
      return (
        Y > 0 && Y < p.length - 1
          ? (G = dS(p[Y - 1], N, p[Y + 1], d))
          : (G = `${Y === 0 ? "M" : "L"}${N.x} ${N.y}`),
        (B += G),
        B
      );
    }, ""),
    x,
    _,
    E,
    D,
  ];
}
const fs = H.memo(
  ({
    sourceX: l,
    sourceY: u,
    targetX: i,
    targetY: s,
    label: o,
    labelStyle: f,
    labelShowBg: d,
    labelBgStyle: g,
    labelBgPadding: y,
    labelBgBorderRadius: m,
    style: p,
    sourcePosition: x = pt.Bottom,
    targetPosition: _ = pt.Top,
    markerEnd: E,
    markerStart: D,
    pathOptions: z,
    interactionWidth: B,
  }) => {
    const [N, Y, G] = lf({
      sourceX: l,
      sourceY: u,
      sourcePosition: x,
      targetX: i,
      targetY: s,
      targetPosition: _,
      borderRadius: z == null ? void 0 : z.borderRadius,
      offset: z == null ? void 0 : z.offset,
    });
    return J.createElement(mi, {
      path: N,
      labelX: Y,
      labelY: G,
      label: o,
      labelStyle: f,
      labelShowBg: d,
      labelBgStyle: g,
      labelBgPadding: y,
      labelBgBorderRadius: m,
      style: p,
      markerEnd: E,
      markerStart: D,
      interactionWidth: B,
    });
  },
);
fs.displayName = "SmoothStepEdge";
const Sf = H.memo((l) => {
  var u;
  return J.createElement(fs, {
    ...l,
    pathOptions: H.useMemo(() => {
      var i;
      return {
        borderRadius: 0,
        offset: (i = l.pathOptions) == null ? void 0 : i.offset,
      };
    }, [(u = l.pathOptions) == null ? void 0 : u.offset]),
  });
});
Sf.displayName = "StepEdge";
function hS({ sourceX: l, sourceY: u, targetX: i, targetY: s }) {
  const [o, f, d, g] = uy({ sourceX: l, sourceY: u, targetX: i, targetY: s });
  return [`M ${l},${u}L ${i},${s}`, o, f, d, g];
}
const Ef = H.memo(
  ({
    sourceX: l,
    sourceY: u,
    targetX: i,
    targetY: s,
    label: o,
    labelStyle: f,
    labelShowBg: d,
    labelBgStyle: g,
    labelBgPadding: y,
    labelBgBorderRadius: m,
    style: p,
    markerEnd: x,
    markerStart: _,
    interactionWidth: E,
  }) => {
    const [D, z, B] = hS({ sourceX: l, sourceY: u, targetX: i, targetY: s });
    return J.createElement(mi, {
      path: D,
      labelX: z,
      labelY: B,
      label: o,
      labelStyle: f,
      labelShowBg: d,
      labelBgStyle: g,
      labelBgPadding: y,
      labelBgBorderRadius: m,
      style: p,
      markerEnd: x,
      markerStart: _,
      interactionWidth: E,
    });
  },
);
Ef.displayName = "StraightEdge";
function Gc(l, u) {
  return l >= 0 ? 0.5 * l : u * 25 * Math.sqrt(-l);
}
function P0({ pos: l, x1: u, y1: i, x2: s, y2: o, c: f }) {
  switch (l) {
    case pt.Left:
      return [u - Gc(u - s, f), i];
    case pt.Right:
      return [u + Gc(s - u, f), i];
    case pt.Top:
      return [u, i - Gc(i - o, f)];
    case pt.Bottom:
      return [u, i + Gc(o - i, f)];
  }
}
function sy({
  sourceX: l,
  sourceY: u,
  sourcePosition: i = pt.Bottom,
  targetX: s,
  targetY: o,
  targetPosition: f = pt.Top,
  curvature: d = 0.25,
}) {
  const [g, y] = P0({ pos: i, x1: l, y1: u, x2: s, y2: o, c: d }),
    [m, p] = P0({ pos: f, x1: s, y1: o, x2: l, y2: u, c: d }),
    [x, _, E, D] = iy({
      sourceX: l,
      sourceY: u,
      targetX: s,
      targetY: o,
      sourceControlX: g,
      sourceControlY: y,
      targetControlX: m,
      targetControlY: p,
    });
  return [`M${l},${u} C${g},${y} ${m},${p} ${s},${o}`, x, _, E, D];
}
const us = H.memo(
  ({
    sourceX: l,
    sourceY: u,
    targetX: i,
    targetY: s,
    sourcePosition: o = pt.Bottom,
    targetPosition: f = pt.Top,
    label: d,
    labelStyle: g,
    labelShowBg: y,
    labelBgStyle: m,
    labelBgPadding: p,
    labelBgBorderRadius: x,
    style: _,
    markerEnd: E,
    markerStart: D,
    pathOptions: z,
    interactionWidth: B,
  }) => {
    const [N, Y, G] = sy({
      sourceX: l,
      sourceY: u,
      sourcePosition: o,
      targetX: i,
      targetY: s,
      targetPosition: f,
      curvature: z == null ? void 0 : z.curvature,
    });
    return J.createElement(mi, {
      path: N,
      labelX: Y,
      labelY: G,
      label: d,
      labelStyle: g,
      labelShowBg: y,
      labelBgStyle: m,
      labelBgPadding: p,
      labelBgBorderRadius: x,
      style: _,
      markerEnd: E,
      markerStart: D,
      interactionWidth: B,
    });
  },
);
us.displayName = "BezierEdge";
const _f = H.createContext(null),
  mS = _f.Provider;
_f.Consumer;
const gS = () => H.useContext(_f),
  yS = (l) => "id" in l && "source" in l && "target" in l,
  pS = ({ source: l, sourceHandle: u, target: i, targetHandle: s }) =>
    `reactflow__edge-${l}${u || ""}-${i}${s || ""}`,
  af = (l, u) =>
    typeof l > "u"
      ? ""
      : typeof l == "string"
        ? l
        : `${u ? `${u}__` : ""}${Object.keys(l)
            .sort()
            .map((s) => `${s}=${l[s]}`)
            .join("&")}`,
  vS = (l, u) =>
    u.some(
      (i) =>
        i.source === l.source &&
        i.target === l.target &&
        (i.sourceHandle === l.sourceHandle ||
          (!i.sourceHandle && !l.sourceHandle)) &&
        (i.targetHandle === l.targetHandle ||
          (!i.targetHandle && !l.targetHandle)),
    ),
  ry = (l, u) => {
    if (!l.source || !l.target) return u;
    let i;
    return (
      yS(l) ? (i = { ...l }) : (i = { ...l, id: pS(l) }),
      vS(i, u) ? u : u.concat(i)
    );
  },
  uf = ({ x: l, y: u }, [i, s, o], f, [d, g]) => {
    const y = { x: (l - i) / o, y: (u - s) / o };
    return f ? { x: d * Math.round(y.x / d), y: g * Math.round(y.y / g) } : y;
  },
  oy = ({ x: l, y: u }, [i, s, o]) => ({ x: l * o + i, y: u * o + s }),
  la = (l, u = [0, 0]) => {
    if (!l) return { x: 0, y: 0, positionAbsolute: { x: 0, y: 0 } };
    const i = (l.width ?? 0) * u[0],
      s = (l.height ?? 0) * u[1],
      o = { x: l.position.x - i, y: l.position.y - s };
    return {
      ...o,
      positionAbsolute: l.positionAbsolute
        ? { x: l.positionAbsolute.x - i, y: l.positionAbsolute.y - s }
        : o,
    };
  },
  ds = (l, u = [0, 0]) => {
    if (l.length === 0) return { x: 0, y: 0, width: 0, height: 0 };
    const i = l.reduce(
      (s, o) => {
        const { x: f, y: d } = la(o, u).positionAbsolute;
        return ey(
          s,
          si({ x: f, y: d, width: o.width || 0, height: o.height || 0 }),
        );
      },
      { x: 1 / 0, y: 1 / 0, x2: -1 / 0, y2: -1 / 0 },
    );
    return ny(i);
  },
  fy = (l, u, [i, s, o] = [0, 0, 1], f = !1, d = !1, g = [0, 0]) => {
    const y = {
        x: (u.x - i) / o,
        y: (u.y - s) / o,
        width: u.width / o,
        height: u.height / o,
      },
      m = [];
    return (
      l.forEach((p) => {
        const { width: x, height: _, selectable: E = !0, hidden: D = !1 } = p;
        if ((d && !E) || D) return !1;
        const { positionAbsolute: z } = la(p, g),
          B = { x: z.x, y: z.y, width: x || 0, height: _ || 0 },
          N = ef(y, B),
          Y = typeof x > "u" || typeof _ > "u" || x === null || _ === null,
          G = f && N > 0,
          j = (x || 0) * (_ || 0);
        (Y || G || N >= j || p.dragging) && m.push(p);
      }),
      m
    );
  },
  dy = (l, u) => {
    const i = l.map((s) => s.id);
    return u.filter((s) => i.includes(s.source) || i.includes(s.target));
  },
  hy = (l, u, i, s, o, f = 0.1) => {
    const d = u / (l.width * (1 + f)),
      g = i / (l.height * (1 + f)),
      y = Math.min(d, g),
      m = Wa(y, s, o),
      p = l.x + l.width / 2,
      x = l.y + l.height / 2,
      _ = u / 2 - p * m,
      E = i / 2 - x * m;
    return { x: _, y: E, zoom: m };
  },
  Pl = (l, u = 0) => l.transition().duration(u);
function tg(l, u, i, s) {
  return (u[i] || []).reduce((o, f) => {
    var d, g;
    return (
      `${l.id}-${f.id}-${i}` !== s &&
        o.push({
          id: f.id || null,
          type: i,
          nodeId: l.id,
          x:
            (((d = l.positionAbsolute) == null ? void 0 : d.x) ?? 0) +
            f.x +
            f.width / 2,
          y:
            (((g = l.positionAbsolute) == null ? void 0 : g.y) ?? 0) +
            f.y +
            f.height / 2,
        }),
      o
    );
  }, []);
}
function xS(l, u, i, s, o, f) {
  const { x: d, y: g } = Cl(l),
    m = u
      .elementsFromPoint(d, g)
      .find((D) => D.classList.contains("react-flow__handle"));
  if (m) {
    const D = m.getAttribute("data-nodeid");
    if (D) {
      const z = wf(void 0, m),
        B = m.getAttribute("data-handleid"),
        N = f({ nodeId: D, id: B, type: z });
      if (N) {
        const Y = o.find((G) => G.nodeId === D && G.type === z && G.id === B);
        return {
          handle: {
            id: B,
            type: z,
            nodeId: D,
            x: (Y == null ? void 0 : Y.x) || i.x,
            y: (Y == null ? void 0 : Y.y) || i.y,
          },
          validHandleResult: N,
        };
      }
    }
  }
  let p = [],
    x = 1 / 0;
  if (
    (o.forEach((D) => {
      const z = Math.sqrt((D.x - i.x) ** 2 + (D.y - i.y) ** 2);
      if (z <= s) {
        const B = f(D);
        z <= x &&
          (z < x
            ? (p = [{ handle: D, validHandleResult: B }])
            : z === x && p.push({ handle: D, validHandleResult: B }),
          (x = z));
      }
    }),
    !p.length)
  )
    return { handle: null, validHandleResult: my() };
  if (p.length === 1) return p[0];
  const _ = p.some(({ validHandleResult: D }) => D.isValid),
    E = p.some(({ handle: D }) => D.type === "target");
  return (
    p.find(({ handle: D, validHandleResult: z }) =>
      E ? D.type === "target" : _ ? z.isValid : !0,
    ) || p[0]
  );
}
const bS = {
    source: null,
    target: null,
    sourceHandle: null,
    targetHandle: null,
  },
  my = () => ({
    handleDomNode: null,
    isValid: !1,
    connection: bS,
    endHandle: null,
  });
function gy(l, u, i, s, o, f, d) {
  const g = o === "target",
    y = d.querySelector(
      `.react-flow__handle[data-id="${l == null ? void 0 : l.nodeId}-${l == null ? void 0 : l.id}-${l == null ? void 0 : l.type}"]`,
    ),
    m = { ...my(), handleDomNode: y };
  if (y) {
    const p = wf(void 0, y),
      x = y.getAttribute("data-nodeid"),
      _ = y.getAttribute("data-handleid"),
      E = y.classList.contains("connectable"),
      D = y.classList.contains("connectableend"),
      z = {
        source: g ? x : i,
        sourceHandle: g ? _ : s,
        target: g ? i : x,
        targetHandle: g ? s : _,
      };
    ((m.connection = z),
      E &&
        D &&
        (u === ua.Strict
          ? (g && p === "source") || (!g && p === "target")
          : x !== i || _ !== s) &&
        ((m.endHandle = { nodeId: x, handleId: _, type: p }),
        (m.isValid = f(z))));
  }
  return m;
}
function SS({ nodes: l, nodeId: u, handleId: i, handleType: s }) {
  return l.reduce((o, f) => {
    if (f[Ft]) {
      const { handleBounds: d } = f[Ft];
      let g = [],
        y = [];
      (d &&
        ((g = tg(f, d, "source", `${u}-${i}-${s}`)),
        (y = tg(f, d, "target", `${u}-${i}-${s}`))),
        o.push(...g, ...y));
    }
    return o;
  }, []);
}
function wf(l, u) {
  return (
    l ||
    (u != null && u.classList.contains("target")
      ? "target"
      : u != null && u.classList.contains("source")
        ? "source"
        : null)
  );
}
function Vo(l) {
  l == null ||
    l.classList.remove(
      "valid",
      "connecting",
      "react-flow__handle-valid",
      "react-flow__handle-connecting",
    );
}
function ES(l, u) {
  let i = null;
  return (u ? (i = "valid") : l && !u && (i = "invalid"), i);
}
function yy({
  event: l,
  handleId: u,
  nodeId: i,
  onConnect: s,
  isTarget: o,
  getState: f,
  setState: d,
  isValidConnection: g,
  edgeUpdaterType: y,
  onReconnectEnd: m,
}) {
  const p = ty(l.target),
    {
      connectionMode: x,
      domNode: _,
      autoPanOnConnect: E,
      connectionRadius: D,
      onConnectStart: z,
      panBy: B,
      getNodes: N,
      cancelConnection: Y,
    } = f();
  let G = 0,
    j;
  const { x: W, y: I } = Cl(l),
    Q = p == null ? void 0 : p.elementFromPoint(W, I),
    ut = wf(y, Q),
    et = _ == null ? void 0 : _.getBoundingClientRect();
  if (!et || !ut) return;
  let ct,
    at = Cl(l, et),
    st = !1,
    rt = null,
    w = !1,
    L = null;
  const S = SS({ nodes: N(), nodeId: i, handleId: u, handleType: ut }),
    R = () => {
      if (!E) return;
      const [k, b] = Pg(at, et);
      (B({ x: k, y: b }), (G = requestAnimationFrame(R)));
    };
  (d({
    connectionPosition: at,
    connectionStatus: null,
    connectionNodeId: i,
    connectionHandleId: u,
    connectionHandleType: ut,
    connectionStartHandle: { nodeId: i, handleId: u, type: ut },
    connectionEndHandle: null,
  }),
    z == null || z(l, { nodeId: i, handleId: u, handleType: ut }));
  function Z(k) {
    const { transform: b } = f();
    at = Cl(k, et);
    const { handle: M, validHandleResult: $ } = xS(
      k,
      p,
      uf(at, b, !1, [1, 1]),
      D,
      S,
      (F) => gy(F, x, i, u, o ? "target" : "source", g, p),
    );
    if (
      ((j = M),
      st || (R(), (st = !0)),
      (L = $.handleDomNode),
      (rt = $.connection),
      (w = $.isValid),
      d({
        connectionPosition: j && w ? oy({ x: j.x, y: j.y }, b) : at,
        connectionStatus: ES(!!j, w),
        connectionEndHandle: $.endHandle,
      }),
      !j && !w && !L)
    )
      return Vo(ct);
    rt.source !== rt.target &&
      L &&
      (Vo(ct),
      (ct = L),
      L.classList.add("connecting", "react-flow__handle-connecting"),
      L.classList.toggle("valid", w),
      L.classList.toggle("react-flow__handle-valid", w));
  }
  function O(k) {
    var b, M;
    ((j || L) && rt && w && (s == null || s(rt)),
      (M = (b = f()).onConnectEnd) == null || M.call(b, k),
      y && (m == null || m(k)),
      Vo(ct),
      Y(),
      cancelAnimationFrame(G),
      (st = !1),
      (w = !1),
      (rt = null),
      (L = null),
      p.removeEventListener("mousemove", Z),
      p.removeEventListener("mouseup", O),
      p.removeEventListener("touchmove", Z),
      p.removeEventListener("touchend", O));
  }
  (p.addEventListener("mousemove", Z),
    p.addEventListener("mouseup", O),
    p.addEventListener("touchmove", Z),
    p.addEventListener("touchend", O));
}
const eg = () => !0,
  _S = (l) => ({
    connectionStartHandle: l.connectionStartHandle,
    connectOnClick: l.connectOnClick,
    noPanClassName: l.noPanClassName,
  }),
  wS = (l, u, i) => (s) => {
    const {
      connectionStartHandle: o,
      connectionEndHandle: f,
      connectionClickStartHandle: d,
    } = s;
    return {
      connecting:
        ((o == null ? void 0 : o.nodeId) === l &&
          (o == null ? void 0 : o.handleId) === u &&
          (o == null ? void 0 : o.type) === i) ||
        ((f == null ? void 0 : f.nodeId) === l &&
          (f == null ? void 0 : f.handleId) === u &&
          (f == null ? void 0 : f.type) === i),
      clickConnecting:
        (d == null ? void 0 : d.nodeId) === l &&
        (d == null ? void 0 : d.handleId) === u &&
        (d == null ? void 0 : d.type) === i,
    };
  },
  py = H.forwardRef(
    (
      {
        type: l = "source",
        position: u = pt.Top,
        isValidConnection: i,
        isConnectable: s = !0,
        isConnectableStart: o = !0,
        isConnectableEnd: f = !0,
        id: d,
        onConnect: g,
        children: y,
        className: m,
        onMouseDown: p,
        onTouchStart: x,
        ..._
      },
      E,
    ) => {
      var et, ct;
      const D = d || null,
        z = l === "target",
        B = fe(),
        N = gS(),
        { connectOnClick: Y, noPanClassName: G } = Gt(_S, me),
        { connecting: j, clickConnecting: W } = Gt(wS(N, D, l), me);
      N ||
        (ct = (et = B.getState()).onError) == null ||
        ct.call(et, "010", Kn.error010());
      const I = (at) => {
          const {
              defaultEdgeOptions: st,
              onConnect: rt,
              hasDefaultEdges: w,
            } = B.getState(),
            L = { ...st, ...at };
          if (w) {
            const { edges: S, setEdges: R } = B.getState();
            R(ry(L, S));
          }
          (rt == null || rt(L), g == null || g(L));
        },
        Q = (at) => {
          if (!N) return;
          const st = ay(at);
          (o &&
            ((st && at.button === 0) || !st) &&
            yy({
              event: at,
              handleId: D,
              nodeId: N,
              onConnect: I,
              isTarget: z,
              getState: B.getState,
              setState: B.setState,
              isValidConnection: i || B.getState().isValidConnection || eg,
            }),
            st ? p == null || p(at) : x == null || x(at));
        },
        ut = (at) => {
          const {
            onClickConnectStart: st,
            onClickConnectEnd: rt,
            connectionClickStartHandle: w,
            connectionMode: L,
            isValidConnection: S,
          } = B.getState();
          if (!N || (!w && !o)) return;
          if (!w) {
            (st == null || st(at, { nodeId: N, handleId: D, handleType: l }),
              B.setState({
                connectionClickStartHandle: { nodeId: N, type: l, handleId: D },
              }));
            return;
          }
          const R = ty(at.target),
            Z = i || S || eg,
            { connection: O, isValid: k } = gy(
              { nodeId: N, id: D, type: l },
              L,
              w.nodeId,
              w.handleId || null,
              w.type,
              Z,
              R,
            );
          (k && I(O),
            rt == null || rt(at),
            B.setState({ connectionClickStartHandle: null }));
        };
      return J.createElement(
        "div",
        {
          "data-handleid": D,
          "data-nodeid": N,
          "data-handlepos": u,
          "data-id": `${N}-${D}-${l}`,
          className: be([
            "react-flow__handle",
            `react-flow__handle-${u}`,
            "nodrag",
            G,
            m,
            {
              source: !z,
              target: z,
              connectable: s,
              connectablestart: o,
              connectableend: f,
              connecting: W,
              connectionindicator: s && ((o && !j) || (f && j)),
            },
          ]),
          onMouseDown: Q,
          onTouchStart: Q,
          onClick: Y ? ut : void 0,
          ref: E,
          ..._,
        },
        y,
      );
    },
  );
py.displayName = "Handle";
var Fa = H.memo(py);
const vy = ({
  data: l,
  isConnectable: u,
  targetPosition: i = pt.Top,
  sourcePosition: s = pt.Bottom,
}) =>
  J.createElement(
    J.Fragment,
    null,
    J.createElement(Fa, { type: "target", position: i, isConnectable: u }),
    l == null ? void 0 : l.label,
    J.createElement(Fa, { type: "source", position: s, isConnectable: u }),
  );
vy.displayName = "DefaultNode";
var cf = H.memo(vy);
const xy = ({ data: l, isConnectable: u, sourcePosition: i = pt.Bottom }) =>
  J.createElement(
    J.Fragment,
    null,
    l == null ? void 0 : l.label,
    J.createElement(Fa, { type: "source", position: i, isConnectable: u }),
  );
xy.displayName = "InputNode";
var by = H.memo(xy);
const Sy = ({ data: l, isConnectable: u, targetPosition: i = pt.Top }) =>
  J.createElement(
    J.Fragment,
    null,
    J.createElement(Fa, { type: "target", position: i, isConnectable: u }),
    l == null ? void 0 : l.label,
  );
Sy.displayName = "OutputNode";
var Ey = H.memo(Sy);
const Nf = () => null;
Nf.displayName = "GroupNode";
const NS = (l) => ({
    selectedNodes: l.getNodes().filter((u) => u.selected),
    selectedEdges: l.edges.filter((u) => u.selected).map((u) => ({ ...u })),
  }),
  Zc = (l) => l.id;
function AS(l, u) {
  return (
    me(l.selectedNodes.map(Zc), u.selectedNodes.map(Zc)) &&
    me(l.selectedEdges.map(Zc), u.selectedEdges.map(Zc))
  );
}
const _y = H.memo(({ onSelectionChange: l }) => {
  const u = fe(),
    { selectedNodes: i, selectedEdges: s } = Gt(NS, AS);
  return (
    H.useEffect(() => {
      const o = { nodes: i, edges: s };
      (l == null || l(o), u.getState().onSelectionChange.forEach((f) => f(o)));
    }, [i, s, l]),
    null
  );
});
_y.displayName = "SelectionListener";
const TS = (l) => !!l.onSelectionChange;
function zS({ onSelectionChange: l }) {
  const u = Gt(TS);
  return l || u ? J.createElement(_y, { onSelectionChange: l }) : null;
}
const MS = (l) => ({
  setNodes: l.setNodes,
  setEdges: l.setEdges,
  setDefaultNodesAndEdges: l.setDefaultNodesAndEdges,
  setMinZoom: l.setMinZoom,
  setMaxZoom: l.setMaxZoom,
  setTranslateExtent: l.setTranslateExtent,
  setNodeExtent: l.setNodeExtent,
  reset: l.reset,
});
function Xa(l, u) {
  H.useEffect(() => {
    typeof l < "u" && u(l);
  }, [l]);
}
function wt(l, u, i) {
  H.useEffect(() => {
    typeof u < "u" && i({ [l]: u });
  }, [u]);
}
const CS = ({
    nodes: l,
    edges: u,
    defaultNodes: i,
    defaultEdges: s,
    onConnect: o,
    onConnectStart: f,
    onConnectEnd: d,
    onClickConnectStart: g,
    onClickConnectEnd: y,
    nodesDraggable: m,
    nodesConnectable: p,
    nodesFocusable: x,
    edgesFocusable: _,
    edgesUpdatable: E,
    elevateNodesOnSelect: D,
    minZoom: z,
    maxZoom: B,
    nodeExtent: N,
    onNodesChange: Y,
    onEdgesChange: G,
    elementsSelectable: j,
    connectionMode: W,
    snapGrid: I,
    snapToGrid: Q,
    translateExtent: ut,
    connectOnClick: et,
    defaultEdgeOptions: ct,
    fitView: at,
    fitViewOptions: st,
    onNodesDelete: rt,
    onEdgesDelete: w,
    onNodeDrag: L,
    onNodeDragStart: S,
    onNodeDragStop: R,
    onSelectionDrag: Z,
    onSelectionDragStart: O,
    onSelectionDragStop: k,
    noPanClassName: b,
    nodeOrigin: M,
    rfId: $,
    autoPanOnConnect: F,
    autoPanOnNodeDrag: it,
    onError: ot,
    connectionRadius: dt,
    isValidConnection: tt,
    nodeDragThreshold: ft,
  }) => {
    const {
        setNodes: ht,
        setEdges: bt,
        setDefaultNodesAndEdges: Nt,
        setMinZoom: ne,
        setMaxZoom: Vt,
        setTranslateExtent: Lt,
        setNodeExtent: se,
        reset: Dt,
      } = Gt(MS, me),
      gt = fe();
    return (
      H.useEffect(() => {
        const It = s == null ? void 0 : s.map((Oe) => ({ ...Oe, ...ct }));
        return (
          Nt(i, It),
          () => {
            Dt();
          }
        );
      }, []),
      wt("defaultEdgeOptions", ct, gt.setState),
      wt("connectionMode", W, gt.setState),
      wt("onConnect", o, gt.setState),
      wt("onConnectStart", f, gt.setState),
      wt("onConnectEnd", d, gt.setState),
      wt("onClickConnectStart", g, gt.setState),
      wt("onClickConnectEnd", y, gt.setState),
      wt("nodesDraggable", m, gt.setState),
      wt("nodesConnectable", p, gt.setState),
      wt("nodesFocusable", x, gt.setState),
      wt("edgesFocusable", _, gt.setState),
      wt("edgesUpdatable", E, gt.setState),
      wt("elementsSelectable", j, gt.setState),
      wt("elevateNodesOnSelect", D, gt.setState),
      wt("snapToGrid", Q, gt.setState),
      wt("snapGrid", I, gt.setState),
      wt("onNodesChange", Y, gt.setState),
      wt("onEdgesChange", G, gt.setState),
      wt("connectOnClick", et, gt.setState),
      wt("fitViewOnInit", at, gt.setState),
      wt("fitViewOnInitOptions", st, gt.setState),
      wt("onNodesDelete", rt, gt.setState),
      wt("onEdgesDelete", w, gt.setState),
      wt("onNodeDrag", L, gt.setState),
      wt("onNodeDragStart", S, gt.setState),
      wt("onNodeDragStop", R, gt.setState),
      wt("onSelectionDrag", Z, gt.setState),
      wt("onSelectionDragStart", O, gt.setState),
      wt("onSelectionDragStop", k, gt.setState),
      wt("noPanClassName", b, gt.setState),
      wt("nodeOrigin", M, gt.setState),
      wt("rfId", $, gt.setState),
      wt("autoPanOnConnect", F, gt.setState),
      wt("autoPanOnNodeDrag", it, gt.setState),
      wt("onError", ot, gt.setState),
      wt("connectionRadius", dt, gt.setState),
      wt("isValidConnection", tt, gt.setState),
      wt("nodeDragThreshold", ft, gt.setState),
      Xa(l, ht),
      Xa(u, bt),
      Xa(z, ne),
      Xa(B, Vt),
      Xa(ut, Lt),
      Xa(N, se),
      null
    );
  },
  ng = { display: "none" },
  DS = {
    position: "absolute",
    width: 1,
    height: 1,
    margin: -1,
    border: 0,
    padding: 0,
    overflow: "hidden",
    clip: "rect(0px, 0px, 0px, 0px)",
    clipPath: "inset(100%)",
  },
  wy = "react-flow__node-desc",
  Ny = "react-flow__edge-desc",
  OS = "react-flow__aria-live",
  RS = (l) => l.ariaLiveMessage;
function HS({ rfId: l }) {
  const u = Gt(RS);
  return J.createElement(
    "div",
    {
      id: `${OS}-${l}`,
      "aria-live": "assertive",
      "aria-atomic": "true",
      style: DS,
    },
    u,
  );
}
function jS({ rfId: l, disableKeyboardA11y: u }) {
  return J.createElement(
    J.Fragment,
    null,
    J.createElement(
      "div",
      { id: `${wy}-${l}`, style: ng },
      "Press enter or space to select a node.",
      !u && "You can then use the arrow keys to move the node around.",
      " Press delete to remove it and escape to cancel.",
      " ",
    ),
    J.createElement(
      "div",
      { id: `${Ny}-${l}`, style: ng },
      "Press enter or space to select an edge. You can then press delete to remove it or escape to cancel.",
    ),
    !u && J.createElement(HS, { rfId: l }),
  );
}
var oi = (l = null, u = { actInsideInputWithModifier: !0 }) => {
  const [i, s] = H.useState(!1),
    o = H.useRef(!1),
    f = H.useRef(new Set([])),
    [d, g] = H.useMemo(() => {
      if (l !== null) {
        const m = (Array.isArray(l) ? l : [l])
            .filter((x) => typeof x == "string")
            .map((x) => x.split("+")),
          p = m.reduce((x, _) => x.concat(..._), []);
        return [m, p];
      }
      return [[], []];
    }, [l]);
  return (
    H.useEffect(() => {
      const y = typeof document < "u" ? document : null,
        m = (u == null ? void 0 : u.target) || y;
      if (l !== null) {
        const p = (E) => {
            if (
              ((o.current = E.ctrlKey || E.metaKey || E.shiftKey),
              (!o.current || (o.current && !u.actInsideInputWithModifier)) &&
                nf(E))
            )
              return !1;
            const z = ag(E.code, g);
            (f.current.add(E[z]),
              lg(d, f.current, !1) && (E.preventDefault(), s(!0)));
          },
          x = (E) => {
            if (
              (!o.current || (o.current && !u.actInsideInputWithModifier)) &&
              nf(E)
            )
              return !1;
            const z = ag(E.code, g);
            (lg(d, f.current, !0)
              ? (s(!1), f.current.clear())
              : f.current.delete(E[z]),
              E.key === "Meta" && f.current.clear(),
              (o.current = !1));
          },
          _ = () => {
            (f.current.clear(), s(!1));
          };
        return (
          m == null || m.addEventListener("keydown", p),
          m == null || m.addEventListener("keyup", x),
          window.addEventListener("blur", _),
          () => {
            (m == null || m.removeEventListener("keydown", p),
              m == null || m.removeEventListener("keyup", x),
              window.removeEventListener("blur", _));
          }
        );
      }
    }, [l, s]),
    i
  );
};
function lg(l, u, i) {
  return l
    .filter((s) => i || s.length === u.size)
    .some((s) => s.every((o) => u.has(o)));
}
function ag(l, u) {
  return u.includes(l) ? "code" : "key";
}
function Ay(l, u, i, s) {
  var g, y;
  const o = l.parentNode || l.parentId;
  if (!o) return i;
  const f = u.get(o),
    d = la(f, s);
  return Ay(
    f,
    u,
    {
      x: (i.x ?? 0) + d.x,
      y: (i.y ?? 0) + d.y,
      z:
        (((g = f[Ft]) == null ? void 0 : g.z) ?? 0) > (i.z ?? 0)
          ? (((y = f[Ft]) == null ? void 0 : y.z) ?? 0)
          : (i.z ?? 0),
    },
    s,
  );
}
function Ty(l, u, i) {
  l.forEach((s) => {
    var f;
    const o = s.parentNode || s.parentId;
    if (o && !l.has(o)) throw new Error(`Parent node ${o} not found`);
    if (o || (i != null && i[s.id])) {
      const {
        x: d,
        y: g,
        z: y,
      } = Ay(
        s,
        l,
        { ...s.position, z: ((f = s[Ft]) == null ? void 0 : f.z) ?? 0 },
        u,
      );
      ((s.positionAbsolute = { x: d, y: g }),
        (s[Ft].z = y),
        i != null && i[s.id] && (s[Ft].isParent = !0));
    }
  });
}
function Lo(l, u, i, s) {
  const o = new Map(),
    f = {},
    d = s ? 1e3 : 0;
  return (
    l.forEach((g) => {
      var E;
      const y = (en(g.zIndex) ? g.zIndex : 0) + (g.selected ? d : 0),
        m = u.get(g.id),
        p = { ...g, positionAbsolute: { x: g.position.x, y: g.position.y } },
        x = g.parentNode || g.parentId;
      x && (f[x] = !0);
      const _ =
        (m == null ? void 0 : m.type) &&
        (m == null ? void 0 : m.type) !== g.type;
      (Object.defineProperty(p, Ft, {
        enumerable: !1,
        value: {
          handleBounds:
            _ || (E = m == null ? void 0 : m[Ft]) == null
              ? void 0
              : E.handleBounds,
          z: y,
        },
      }),
        o.set(g.id, p));
    }),
    Ty(o, i, f),
    o
  );
}
function zy(l, u = {}) {
  const {
      getNodes: i,
      width: s,
      height: o,
      minZoom: f,
      maxZoom: d,
      d3Zoom: g,
      d3Selection: y,
      fitViewOnInitDone: m,
      fitViewOnInit: p,
      nodeOrigin: x,
    } = l(),
    _ = u.initial && !m && p;
  if (g && y && (_ || !u.initial)) {
    const D = i().filter((B) => {
        var Y;
        const N = u.includeHiddenNodes ? B.width && B.height : !B.hidden;
        return (Y = u.nodes) != null && Y.length
          ? N && u.nodes.some((G) => G.id === B.id)
          : N;
      }),
      z = D.every((B) => B.width && B.height);
    if (D.length > 0 && z) {
      const B = ds(D, x),
        {
          x: N,
          y: Y,
          zoom: G,
        } = hy(B, s, o, u.minZoom ?? f, u.maxZoom ?? d, u.padding ?? 0.1),
        j = Qn.translate(N, Y).scale(G);
      return (
        typeof u.duration == "number" && u.duration > 0
          ? g.transform(Pl(y, u.duration), j)
          : g.transform(y, j),
        !0
      );
    }
  }
  return !1;
}
function US(l, u) {
  return (
    l.forEach((i) => {
      const s = u.get(i.id);
      s && u.set(s.id, { ...s, [Ft]: s[Ft], selected: i.selected });
    }),
    new Map(u)
  );
}
function BS(l, u) {
  return u.map((i) => {
    const s = l.find((o) => o.id === i.id);
    return (s && (i.selected = s.selected), i);
  });
}
function Qc({ changedNodes: l, changedEdges: u, get: i, set: s }) {
  const {
    nodeInternals: o,
    edges: f,
    onNodesChange: d,
    onEdgesChange: g,
    hasDefaultNodes: y,
    hasDefaultEdges: m,
  } = i();
  (l != null &&
    l.length &&
    (y && s({ nodeInternals: US(l, o) }), d == null || d(l)),
    u != null && u.length && (m && s({ edges: BS(u, f) }), g == null || g(u)));
}
const Va = () => {},
  YS = {
    zoomIn: Va,
    zoomOut: Va,
    zoomTo: Va,
    getZoom: () => 1,
    setViewport: Va,
    getViewport: () => ({ x: 0, y: 0, zoom: 1 }),
    fitView: () => !1,
    setCenter: Va,
    fitBounds: Va,
    project: (l) => l,
    screenToFlowPosition: (l) => l,
    flowToScreenPosition: (l) => l,
    viewportInitialized: !1,
  },
  qS = (l) => ({ d3Zoom: l.d3Zoom, d3Selection: l.d3Selection }),
  XS = () => {
    const l = fe(),
      { d3Zoom: u, d3Selection: i } = Gt(qS, me);
    return H.useMemo(
      () =>
        i && u
          ? {
              zoomIn: (o) =>
                u.scaleBy(Pl(i, o == null ? void 0 : o.duration), 1.2),
              zoomOut: (o) =>
                u.scaleBy(Pl(i, o == null ? void 0 : o.duration), 1 / 1.2),
              zoomTo: (o, f) =>
                u.scaleTo(Pl(i, f == null ? void 0 : f.duration), o),
              getZoom: () => l.getState().transform[2],
              setViewport: (o, f) => {
                const [d, g, y] = l.getState().transform,
                  m = Qn.translate(o.x ?? d, o.y ?? g).scale(o.zoom ?? y);
                u.transform(Pl(i, f == null ? void 0 : f.duration), m);
              },
              getViewport: () => {
                const [o, f, d] = l.getState().transform;
                return { x: o, y: f, zoom: d };
              },
              fitView: (o) => zy(l.getState, o),
              setCenter: (o, f, d) => {
                const { width: g, height: y, maxZoom: m } = l.getState(),
                  p = typeof (d == null ? void 0 : d.zoom) < "u" ? d.zoom : m,
                  x = g / 2 - o * p,
                  _ = y / 2 - f * p,
                  E = Qn.translate(x, _).scale(p);
                u.transform(Pl(i, d == null ? void 0 : d.duration), E);
              },
              fitBounds: (o, f) => {
                const {
                    width: d,
                    height: g,
                    minZoom: y,
                    maxZoom: m,
                  } = l.getState(),
                  {
                    x: p,
                    y: x,
                    zoom: _,
                  } = hy(
                    o,
                    d,
                    g,
                    y,
                    m,
                    (f == null ? void 0 : f.padding) ?? 0.1,
                  ),
                  E = Qn.translate(p, x).scale(_);
                u.transform(Pl(i, f == null ? void 0 : f.duration), E);
              },
              project: (o) => {
                const {
                  transform: f,
                  snapToGrid: d,
                  snapGrid: g,
                } = l.getState();
                return (
                  console.warn(
                    "[DEPRECATED] `project` is deprecated. Instead use `screenToFlowPosition`. There is no need to subtract the react flow bounds anymore! https://reactflow.dev/api-reference/types/react-flow-instance#screen-to-flow-position",
                  ),
                  uf(o, f, d, g)
                );
              },
              screenToFlowPosition: (o) => {
                const {
                  transform: f,
                  snapToGrid: d,
                  snapGrid: g,
                  domNode: y,
                } = l.getState();
                if (!y) return o;
                const { x: m, y: p } = y.getBoundingClientRect(),
                  x = { x: o.x - m, y: o.y - p };
                return uf(x, f, d, g);
              },
              flowToScreenPosition: (o) => {
                const { transform: f, domNode: d } = l.getState();
                if (!d) return o;
                const { x: g, y } = d.getBoundingClientRect(),
                  m = oy(o, f);
                return { x: m.x + g, y: m.y + y };
              },
              viewportInitialized: !0,
            }
          : YS,
      [u, i],
    );
  };
function Af() {
  const l = XS(),
    u = fe(),
    i = H.useCallback(
      () =>
        u
          .getState()
          .getNodes()
          .map((z) => ({ ...z })),
      [],
    ),
    s = H.useCallback((z) => u.getState().nodeInternals.get(z), []),
    o = H.useCallback(() => {
      const { edges: z = [] } = u.getState();
      return z.map((B) => ({ ...B }));
    }, []),
    f = H.useCallback((z) => {
      const { edges: B = [] } = u.getState();
      return B.find((N) => N.id === z);
    }, []),
    d = H.useCallback((z) => {
      const {
          getNodes: B,
          setNodes: N,
          hasDefaultNodes: Y,
          onNodesChange: G,
        } = u.getState(),
        j = B(),
        W = typeof z == "function" ? z(j) : z;
      if (Y) N(W);
      else if (G) {
        const I =
          W.length === 0
            ? j.map((Q) => ({ type: "remove", id: Q.id }))
            : W.map((Q) => ({ item: Q, type: "reset" }));
        G(I);
      }
    }, []),
    g = H.useCallback((z) => {
      const {
          edges: B = [],
          setEdges: N,
          hasDefaultEdges: Y,
          onEdgesChange: G,
        } = u.getState(),
        j = typeof z == "function" ? z(B) : z;
      if (Y) N(j);
      else if (G) {
        const W =
          j.length === 0
            ? B.map((I) => ({ type: "remove", id: I.id }))
            : j.map((I) => ({ item: I, type: "reset" }));
        G(W);
      }
    }, []),
    y = H.useCallback((z) => {
      const B = Array.isArray(z) ? z : [z],
        {
          getNodes: N,
          setNodes: Y,
          hasDefaultNodes: G,
          onNodesChange: j,
        } = u.getState();
      if (G) {
        const I = [...N(), ...B];
        Y(I);
      } else if (j) {
        const W = B.map((I) => ({ item: I, type: "add" }));
        j(W);
      }
    }, []),
    m = H.useCallback((z) => {
      const B = Array.isArray(z) ? z : [z],
        {
          edges: N = [],
          setEdges: Y,
          hasDefaultEdges: G,
          onEdgesChange: j,
        } = u.getState();
      if (G) Y([...N, ...B]);
      else if (j) {
        const W = B.map((I) => ({ item: I, type: "add" }));
        j(W);
      }
    }, []),
    p = H.useCallback(() => {
      const { getNodes: z, edges: B = [], transform: N } = u.getState(),
        [Y, G, j] = N;
      return {
        nodes: z().map((W) => ({ ...W })),
        edges: B.map((W) => ({ ...W })),
        viewport: { x: Y, y: G, zoom: j },
      };
    }, []),
    x = H.useCallback(({ nodes: z, edges: B }) => {
      const {
          nodeInternals: N,
          getNodes: Y,
          edges: G,
          hasDefaultNodes: j,
          hasDefaultEdges: W,
          onNodesDelete: I,
          onEdgesDelete: Q,
          onNodesChange: ut,
          onEdgesChange: et,
        } = u.getState(),
        ct = (z || []).map((L) => L.id),
        at = (B || []).map((L) => L.id),
        st = Y().reduce((L, S) => {
          const R = S.parentNode || S.parentId,
            Z = !ct.includes(S.id) && R && L.find((k) => k.id === R);
          return (
            (typeof S.deletable == "boolean" ? S.deletable : !0) &&
              (ct.includes(S.id) || Z) &&
              L.push(S),
            L
          );
        }, []),
        rt = G.filter((L) =>
          typeof L.deletable == "boolean" ? L.deletable : !0,
        ),
        w = rt.filter((L) => at.includes(L.id));
      if (st || w) {
        const L = dy(st, rt),
          S = [...w, ...L],
          R = S.reduce((Z, O) => (Z.includes(O.id) || Z.push(O.id), Z), []);
        if (
          ((W || j) &&
            (W && u.setState({ edges: G.filter((Z) => !R.includes(Z.id)) }),
            j &&
              (st.forEach((Z) => {
                N.delete(Z.id);
              }),
              u.setState({ nodeInternals: new Map(N) }))),
          R.length > 0 &&
            (Q == null || Q(S),
            et && et(R.map((Z) => ({ id: Z, type: "remove" })))),
          st.length > 0 && (I == null || I(st), ut))
        ) {
          const Z = st.map((O) => ({ id: O.id, type: "remove" }));
          ut(Z);
        }
      }
    }, []),
    _ = H.useCallback((z) => {
      const B = cS(z),
        N = B ? null : u.getState().nodeInternals.get(z.id);
      return !B && !N ? [null, null, B] : [B ? z : J0(N), N, B];
    }, []),
    E = H.useCallback((z, B = !0, N) => {
      const [Y, G, j] = _(z);
      return Y
        ? (N || u.getState().getNodes()).filter((W) => {
            if (!j && (W.id === G.id || !W.positionAbsolute)) return !1;
            const I = J0(W),
              Q = ef(I, Y);
            return (B && Q > 0) || Q >= Y.width * Y.height;
          })
        : [];
    }, []),
    D = H.useCallback((z, B, N = !0) => {
      const [Y] = _(z);
      if (!Y) return !1;
      const G = ef(Y, B);
      return (N && G > 0) || G >= Y.width * Y.height;
    }, []);
  return H.useMemo(
    () => ({
      ...l,
      getNodes: i,
      getNode: s,
      getEdges: o,
      getEdge: f,
      setNodes: d,
      setEdges: g,
      addNodes: y,
      addEdges: m,
      toObject: p,
      deleteElements: x,
      getIntersectingNodes: E,
      isNodeIntersecting: D,
    }),
    [l, i, s, o, f, d, g, y, m, p, x, E, D],
  );
}
const VS = { actInsideInputWithModifier: !1 };
var LS = ({ deleteKeyCode: l, multiSelectionKeyCode: u }) => {
  const i = fe(),
    { deleteElements: s } = Af(),
    o = oi(l, VS),
    f = oi(u);
  (H.useEffect(() => {
    if (o) {
      const { edges: d, getNodes: g } = i.getState(),
        y = g().filter((p) => p.selected),
        m = d.filter((p) => p.selected);
      (s({ nodes: y, edges: m }), i.setState({ nodesSelectionActive: !1 }));
    }
  }, [o]),
    H.useEffect(() => {
      i.setState({ multiSelectionActive: f });
    }, [f]));
};
function GS(l) {
  const u = fe();
  H.useEffect(() => {
    let i;
    const s = () => {
      var f, d;
      if (!l.current) return;
      const o = vf(l.current);
      ((o.height === 0 || o.width === 0) &&
        ((d = (f = u.getState()).onError) == null ||
          d.call(f, "004", Kn.error004())),
        u.setState({ width: o.width || 500, height: o.height || 500 }));
    };
    return (
      s(),
      window.addEventListener("resize", s),
      l.current && ((i = new ResizeObserver(() => s())), i.observe(l.current)),
      () => {
        (window.removeEventListener("resize", s),
          i && l.current && i.unobserve(l.current));
      }
    );
  }, []);
}
const Tf = {
    position: "absolute",
    width: "100%",
    height: "100%",
    top: 0,
    left: 0,
  },
  ZS = (l, u) => l.x !== u.x || l.y !== u.y || l.zoom !== u.k,
  kc = (l) => ({ x: l.x, y: l.y, zoom: l.k }),
  La = (l, u) => l.target.closest(`.${u}`),
  ug = (l, u) => u === 2 && Array.isArray(l) && l.includes(2),
  ig = (l) => {
    const u = l.ctrlKey && ls() ? 10 : 1;
    return -l.deltaY * (l.deltaMode === 1 ? 0.05 : l.deltaMode ? 1 : 0.002) * u;
  },
  QS = (l) => ({
    d3Zoom: l.d3Zoom,
    d3Selection: l.d3Selection,
    d3ZoomHandler: l.d3ZoomHandler,
    userSelectionActive: l.userSelectionActive,
  }),
  kS = ({
    onMove: l,
    onMoveStart: u,
    onMoveEnd: i,
    onPaneContextMenu: s,
    zoomOnScroll: o = !0,
    zoomOnPinch: f = !0,
    panOnScroll: d = !1,
    panOnScrollSpeed: g = 0.5,
    panOnScrollMode: y = ea.Free,
    zoomOnDoubleClick: m = !0,
    elementsSelectable: p,
    panOnDrag: x = !0,
    defaultViewport: _,
    translateExtent: E,
    minZoom: D,
    maxZoom: z,
    zoomActivationKeyCode: B,
    preventScrolling: N = !0,
    children: Y,
    noWheelClassName: G,
    noPanClassName: j,
  }) => {
    const W = H.useRef(),
      I = fe(),
      Q = H.useRef(!1),
      ut = H.useRef(!1),
      et = H.useRef(null),
      ct = H.useRef({ x: 0, y: 0, zoom: 0 }),
      {
        d3Zoom: at,
        d3Selection: st,
        d3ZoomHandler: rt,
        userSelectionActive: w,
      } = Gt(QS, me),
      L = oi(B),
      S = H.useRef(0),
      R = H.useRef(!1),
      Z = H.useRef();
    return (
      GS(et),
      H.useEffect(() => {
        if (et.current) {
          const O = et.current.getBoundingClientRect(),
            k = Fg().scaleExtent([D, z]).translateExtent(E),
            b = tn(et.current).call(k),
            M = Qn.translate(_.x, _.y).scale(Wa(_.zoom, D, z)),
            $ = [
              [0, 0],
              [O.width, O.height],
            ],
            F = k.constrain()(M, $, E);
          (k.transform(b, F),
            k.wheelDelta(ig),
            I.setState({
              d3Zoom: k,
              d3Selection: b,
              d3ZoomHandler: b.on("wheel.zoom"),
              transform: [F.x, F.y, F.k],
              domNode: et.current.closest(".react-flow"),
            }));
        }
      }, []),
      H.useEffect(() => {
        st &&
          at &&
          (d && !L && !w
            ? st.on(
                "wheel.zoom",
                (O) => {
                  if (La(O, G)) return !1;
                  (O.preventDefault(), O.stopImmediatePropagation());
                  const k = st.property("__zoom").k || 1;
                  if (O.ctrlKey && f) {
                    const tt = cn(O),
                      ft = ig(O),
                      ht = k * Math.pow(2, ft);
                    at.scaleTo(st, ht, tt, O);
                    return;
                  }
                  const b = O.deltaMode === 1 ? 20 : 1;
                  let M = y === ea.Vertical ? 0 : O.deltaX * b,
                    $ = y === ea.Horizontal ? 0 : O.deltaY * b;
                  (!ls() &&
                    O.shiftKey &&
                    y !== ea.Vertical &&
                    ((M = O.deltaY * b), ($ = 0)),
                    at.translateBy(st, -(M / k) * g, -($ / k) * g, {
                      internal: !0,
                    }));
                  const F = kc(st.property("__zoom")),
                    {
                      onViewportChangeStart: it,
                      onViewportChange: ot,
                      onViewportChangeEnd: dt,
                    } = I.getState();
                  (clearTimeout(Z.current),
                    R.current ||
                      ((R.current = !0),
                      u == null || u(O, F),
                      it == null || it(F)),
                    R.current &&
                      (l == null || l(O, F),
                      ot == null || ot(F),
                      (Z.current = setTimeout(() => {
                        (i == null || i(O, F),
                          dt == null || dt(F),
                          (R.current = !1));
                      }, 150))));
                },
                { passive: !1 },
              )
            : typeof rt < "u" &&
              st.on(
                "wheel.zoom",
                function (O, k) {
                  if ((!N && O.type === "wheel" && !O.ctrlKey) || La(O, G))
                    return null;
                  (O.preventDefault(), rt.call(this, O, k));
                },
                { passive: !1 },
              ));
      }, [w, d, y, st, at, rt, L, f, N, G, u, l, i]),
      H.useEffect(() => {
        at &&
          at.on("start", (O) => {
            var M, $;
            if (!O.sourceEvent || O.sourceEvent.internal) return null;
            S.current = (M = O.sourceEvent) == null ? void 0 : M.button;
            const { onViewportChangeStart: k } = I.getState(),
              b = kc(O.transform);
            ((Q.current = !0),
              (ct.current = b),
              (($ = O.sourceEvent) == null ? void 0 : $.type) === "mousedown" &&
                I.setState({ paneDragging: !0 }),
              k == null || k(b),
              u == null || u(O.sourceEvent, b));
          });
      }, [at, u]),
      H.useEffect(() => {
        at &&
          (w && !Q.current
            ? at.on("zoom", null)
            : w ||
              at.on("zoom", (O) => {
                var b;
                const { onViewportChange: k } = I.getState();
                if (
                  (I.setState({
                    transform: [O.transform.x, O.transform.y, O.transform.k],
                  }),
                  (ut.current = !!(s && ug(x, S.current ?? 0))),
                  (l || k) && !((b = O.sourceEvent) != null && b.internal))
                ) {
                  const M = kc(O.transform);
                  (k == null || k(M), l == null || l(O.sourceEvent, M));
                }
              }));
      }, [w, at, l, x, s]),
      H.useEffect(() => {
        at &&
          at.on("end", (O) => {
            if (!O.sourceEvent || O.sourceEvent.internal) return null;
            const { onViewportChangeEnd: k } = I.getState();
            if (
              ((Q.current = !1),
              I.setState({ paneDragging: !1 }),
              s && ug(x, S.current ?? 0) && !ut.current && s(O.sourceEvent),
              (ut.current = !1),
              (i || k) && ZS(ct.current, O.transform))
            ) {
              const b = kc(O.transform);
              ((ct.current = b),
                clearTimeout(W.current),
                (W.current = setTimeout(
                  () => {
                    (k == null || k(b), i == null || i(O.sourceEvent, b));
                  },
                  d ? 150 : 0,
                )));
            }
          });
      }, [at, d, x, i, s]),
      H.useEffect(() => {
        at &&
          at.filter((O) => {
            const k = L || o,
              b = f && O.ctrlKey;
            if (
              (x === !0 || (Array.isArray(x) && x.includes(1))) &&
              O.button === 1 &&
              O.type === "mousedown" &&
              (La(O, "react-flow__node") || La(O, "react-flow__edge"))
            )
              return !0;
            if (
              (!x && !k && !d && !m && !f) ||
              w ||
              (!m && O.type === "dblclick") ||
              (La(O, G) && O.type === "wheel") ||
              (La(O, j) &&
                (O.type !== "wheel" || (d && O.type === "wheel" && !L))) ||
              (!f && O.ctrlKey && O.type === "wheel") ||
              (!k && !d && !b && O.type === "wheel") ||
              (!x && (O.type === "mousedown" || O.type === "touchstart")) ||
              (Array.isArray(x) &&
                !x.includes(O.button) &&
                O.type === "mousedown")
            )
              return !1;
            const M =
              (Array.isArray(x) && x.includes(O.button)) ||
              !O.button ||
              O.button <= 1;
            return (!O.ctrlKey || O.type === "wheel") && M;
          });
      }, [w, at, o, f, d, m, x, p, L]),
      J.createElement(
        "div",
        { className: "react-flow__renderer", ref: et, style: Tf },
        Y,
      )
    );
  },
  KS = (l) => ({
    userSelectionActive: l.userSelectionActive,
    userSelectionRect: l.userSelectionRect,
  });
function $S() {
  const { userSelectionActive: l, userSelectionRect: u } = Gt(KS, me);
  return l && u
    ? J.createElement("div", {
        className: "react-flow__selection react-flow__container",
        style: {
          width: u.width,
          height: u.height,
          transform: `translate(${u.x}px, ${u.y}px)`,
        },
      })
    : null;
}
function cg(l, u) {
  const i = u.parentNode || u.parentId,
    s = l.find((o) => o.id === i);
  if (s) {
    const o = u.position.x + u.width - s.width,
      f = u.position.y + u.height - s.height;
    if (o > 0 || f > 0 || u.position.x < 0 || u.position.y < 0) {
      if (
        ((s.style = { ...s.style }),
        (s.style.width = s.style.width ?? s.width),
        (s.style.height = s.style.height ?? s.height),
        o > 0 && (s.style.width += o),
        f > 0 && (s.style.height += f),
        u.position.x < 0)
      ) {
        const d = Math.abs(u.position.x);
        ((s.position.x = s.position.x - d),
          (s.style.width += d),
          (u.position.x = 0));
      }
      if (u.position.y < 0) {
        const d = Math.abs(u.position.y);
        ((s.position.y = s.position.y - d),
          (s.style.height += d),
          (u.position.y = 0));
      }
      ((s.width = s.style.width), (s.height = s.style.height));
    }
  }
}
function My(l, u) {
  if (l.some((s) => s.type === "reset"))
    return l.filter((s) => s.type === "reset").map((s) => s.item);
  const i = l.filter((s) => s.type === "add").map((s) => s.item);
  return u.reduce((s, o) => {
    const f = l.filter((g) => g.id === o.id);
    if (f.length === 0) return (s.push(o), s);
    const d = { ...o };
    for (const g of f)
      if (g)
        switch (g.type) {
          case "select": {
            d.selected = g.selected;
            break;
          }
          case "position": {
            (typeof g.position < "u" && (d.position = g.position),
              typeof g.positionAbsolute < "u" &&
                (d.positionAbsolute = g.positionAbsolute),
              typeof g.dragging < "u" && (d.dragging = g.dragging),
              d.expandParent && cg(s, d));
            break;
          }
          case "dimensions": {
            (typeof g.dimensions < "u" &&
              ((d.width = g.dimensions.width),
              (d.height = g.dimensions.height)),
              typeof g.updateStyle < "u" &&
                (d.style = { ...(d.style || {}), ...g.dimensions }),
              typeof g.resizing == "boolean" && (d.resizing = g.resizing),
              d.expandParent && cg(s, d));
            break;
          }
          case "remove":
            return s;
        }
    return (s.push(d), s);
  }, i);
}
function Cy(l, u) {
  return My(l, u);
}
function JS(l, u) {
  return My(l, u);
}
const zl = (l, u) => ({ id: l, type: "select", selected: u });
function Za(l, u) {
  return l.reduce((i, s) => {
    const o = u.includes(s.id);
    return (
      !s.selected && o
        ? ((s.selected = !0), i.push(zl(s.id, !0)))
        : s.selected && !o && ((s.selected = !1), i.push(zl(s.id, !1))),
      i
    );
  }, []);
}
const Go = (l, u) => (i) => {
    i.target === u.current && (l == null || l(i));
  },
  WS = (l) => ({
    userSelectionActive: l.userSelectionActive,
    elementsSelectable: l.elementsSelectable,
    dragging: l.paneDragging,
  }),
  Dy = H.memo(
    ({
      isSelecting: l,
      selectionMode: u = ri.Full,
      panOnDrag: i,
      onSelectionStart: s,
      onSelectionEnd: o,
      onPaneClick: f,
      onPaneContextMenu: d,
      onPaneScroll: g,
      onPaneMouseEnter: y,
      onPaneMouseMove: m,
      onPaneMouseLeave: p,
      children: x,
    }) => {
      const _ = H.useRef(null),
        E = fe(),
        D = H.useRef(0),
        z = H.useRef(0),
        B = H.useRef(),
        {
          userSelectionActive: N,
          elementsSelectable: Y,
          dragging: G,
        } = Gt(WS, me),
        j = () => {
          (E.setState({ userSelectionActive: !1, userSelectionRect: null }),
            (D.current = 0),
            (z.current = 0));
        },
        W = (rt) => {
          (f == null || f(rt),
            E.getState().resetSelectedElements(),
            E.setState({ nodesSelectionActive: !1 }));
        },
        I = (rt) => {
          if (Array.isArray(i) && i != null && i.includes(2)) {
            rt.preventDefault();
            return;
          }
          d == null || d(rt);
        },
        Q = g ? (rt) => g(rt) : void 0,
        ut = (rt) => {
          const { resetSelectedElements: w, domNode: L } = E.getState();
          if (
            ((B.current = L == null ? void 0 : L.getBoundingClientRect()),
            !Y ||
              !l ||
              rt.button !== 0 ||
              rt.target !== _.current ||
              !B.current)
          )
            return;
          const { x: S, y: R } = Cl(rt, B.current);
          (w(),
            E.setState({
              userSelectionRect: {
                width: 0,
                height: 0,
                startX: S,
                startY: R,
                x: S,
                y: R,
              },
            }),
            s == null || s(rt));
        },
        et = (rt) => {
          const {
            userSelectionRect: w,
            nodeInternals: L,
            edges: S,
            transform: R,
            onNodesChange: Z,
            onEdgesChange: O,
            nodeOrigin: k,
            getNodes: b,
          } = E.getState();
          if (!l || !B.current || !w) return;
          E.setState({ userSelectionActive: !0, nodesSelectionActive: !1 });
          const M = Cl(rt, B.current),
            $ = w.startX ?? 0,
            F = w.startY ?? 0,
            it = {
              ...w,
              x: M.x < $ ? M.x : $,
              y: M.y < F ? M.y : F,
              width: Math.abs(M.x - $),
              height: Math.abs(M.y - F),
            },
            ot = b(),
            dt = fy(L, it, R, u === ri.Partial, !0, k),
            tt = dy(dt, S).map((ht) => ht.id),
            ft = dt.map((ht) => ht.id);
          if (D.current !== ft.length) {
            D.current = ft.length;
            const ht = Za(ot, ft);
            ht.length && (Z == null || Z(ht));
          }
          if (z.current !== tt.length) {
            z.current = tt.length;
            const ht = Za(S, tt);
            ht.length && (O == null || O(ht));
          }
          E.setState({ userSelectionRect: it });
        },
        ct = (rt) => {
          if (rt.button !== 0) return;
          const { userSelectionRect: w } = E.getState();
          (!N && w && rt.target === _.current && (W == null || W(rt)),
            E.setState({ nodesSelectionActive: D.current > 0 }),
            j(),
            o == null || o(rt));
        },
        at = (rt) => {
          (N &&
            (E.setState({ nodesSelectionActive: D.current > 0 }),
            o == null || o(rt)),
            j());
        },
        st = Y && (l || N);
      return J.createElement(
        "div",
        {
          className: be(["react-flow__pane", { dragging: G, selection: l }]),
          onClick: st ? void 0 : Go(W, _),
          onContextMenu: Go(I, _),
          onWheel: Go(Q, _),
          onMouseEnter: st ? void 0 : y,
          onMouseDown: st ? ut : void 0,
          onMouseMove: st ? et : m,
          onMouseUp: st ? ct : void 0,
          onMouseLeave: st ? at : p,
          ref: _,
          style: Tf,
        },
        x,
        J.createElement($S, null),
      );
    },
  );
Dy.displayName = "Pane";
function Oy(l, u) {
  const i = l.parentNode || l.parentId;
  if (!i) return !1;
  const s = u.get(i);
  return s ? (s.selected ? !0 : Oy(s, u)) : !1;
}
function sg(l, u, i) {
  let s = l;
  do {
    if (s != null && s.matches(u)) return !0;
    if (s === i.current) return !1;
    s = s.parentElement;
  } while (s);
  return !1;
}
function FS(l, u, i, s) {
  return Array.from(l.values())
    .filter(
      (o) =>
        (o.selected || o.id === s) &&
        (!o.parentNode || o.parentId || !Oy(o, l)) &&
        (o.draggable || (u && typeof o.draggable > "u")),
    )
    .map((o) => {
      var f, d;
      return {
        id: o.id,
        position: o.position || { x: 0, y: 0 },
        positionAbsolute: o.positionAbsolute || { x: 0, y: 0 },
        distance: {
          x: i.x - (((f = o.positionAbsolute) == null ? void 0 : f.x) ?? 0),
          y: i.y - (((d = o.positionAbsolute) == null ? void 0 : d.y) ?? 0),
        },
        delta: { x: 0, y: 0 },
        extent: o.extent,
        parentNode: o.parentNode || o.parentId,
        parentId: o.parentNode || o.parentId,
        width: o.width,
        height: o.height,
        expandParent: o.expandParent,
      };
    });
}
function IS(l, u) {
  return !u || u === "parent"
    ? u
    : [u[0], [u[1][0] - (l.width || 0), u[1][1] - (l.height || 0)]];
}
function Ry(l, u, i, s, o = [0, 0], f) {
  const d = IS(l, l.extent || s);
  let g = d;
  const y = l.parentNode || l.parentId;
  if (l.extent === "parent" && !l.expandParent)
    if (y && l.width && l.height) {
      const x = i.get(y),
        { x: _, y: E } = la(x, o).positionAbsolute;
      g =
        x && en(_) && en(E) && en(x.width) && en(x.height)
          ? [
              [_ + l.width * o[0], E + l.height * o[1]],
              [
                _ + x.width - l.width + l.width * o[0],
                E + x.height - l.height + l.height * o[1],
              ],
            ]
          : g;
    } else (f == null || f("005", Kn.error005()), (g = d));
  else if (l.extent && y && l.extent !== "parent") {
    const x = i.get(y),
      { x: _, y: E } = la(x, o).positionAbsolute;
    g = [
      [l.extent[0][0] + _, l.extent[0][1] + E],
      [l.extent[1][0] + _, l.extent[1][1] + E],
    ];
  }
  let m = { x: 0, y: 0 };
  if (y) {
    const x = i.get(y);
    m = la(x, o).positionAbsolute;
  }
  const p = g && g !== "parent" ? xf(u, g) : u;
  return { position: { x: p.x - m.x, y: p.y - m.y }, positionAbsolute: p };
}
function Zo({ nodeId: l, dragItems: u, nodeInternals: i }) {
  const s = u.map((o) => ({
    ...i.get(o.id),
    position: o.position,
    positionAbsolute: o.positionAbsolute,
  }));
  return [l ? s.find((o) => o.id === l) : s[0], s];
}
const rg = (l, u, i, s) => {
  const o = u.querySelectorAll(l);
  if (!o || !o.length) return null;
  const f = Array.from(o),
    d = u.getBoundingClientRect(),
    g = { x: d.width * s[0], y: d.height * s[1] };
  return f.map((y) => {
    const m = y.getBoundingClientRect();
    return {
      id: y.getAttribute("data-handleid"),
      position: y.getAttribute("data-handlepos"),
      x: (m.left - d.left - g.x) / i,
      y: (m.top - d.top - g.y) / i,
      ...vf(y),
    };
  });
};
function Iu(l, u, i) {
  return i === void 0
    ? i
    : (s) => {
        const o = u().nodeInternals.get(l);
        o && i(s, { ...o });
      };
}
function sf({ id: l, store: u, unselect: i = !1, nodeRef: s }) {
  const {
      addSelectedNodes: o,
      unselectNodesAndEdges: f,
      multiSelectionActive: d,
      nodeInternals: g,
      onError: y,
    } = u.getState(),
    m = g.get(l);
  if (!m) {
    y == null || y("012", Kn.error012(l));
    return;
  }
  (u.setState({ nodesSelectionActive: !1 }),
    m.selected
      ? (i || (m.selected && d)) &&
        (f({ nodes: [m], edges: [] }),
        requestAnimationFrame(() => {
          var p;
          return (p = s == null ? void 0 : s.current) == null
            ? void 0
            : p.blur();
        }))
      : o([l]));
}
function PS() {
  const l = fe();
  return H.useCallback(({ sourceEvent: i }) => {
    const { transform: s, snapGrid: o, snapToGrid: f } = l.getState(),
      d = i.touches ? i.touches[0].clientX : i.clientX,
      g = i.touches ? i.touches[0].clientY : i.clientY,
      y = { x: (d - s[0]) / s[2], y: (g - s[1]) / s[2] };
    return {
      xSnapped: f ? o[0] * Math.round(y.x / o[0]) : y.x,
      ySnapped: f ? o[1] * Math.round(y.y / o[1]) : y.y,
      ...y,
    };
  }, []);
}
function Qo(l) {
  return (u, i, s) => (l == null ? void 0 : l(u, s));
}
function Hy({
  nodeRef: l,
  disabled: u = !1,
  noDragClassName: i,
  handleSelector: s,
  nodeId: o,
  isSelectable: f,
  selectNodesOnDrag: d,
}) {
  const g = fe(),
    [y, m] = H.useState(!1),
    p = H.useRef([]),
    x = H.useRef({ x: null, y: null }),
    _ = H.useRef(0),
    E = H.useRef(null),
    D = H.useRef({ x: 0, y: 0 }),
    z = H.useRef(null),
    B = H.useRef(!1),
    N = H.useRef(!1),
    Y = H.useRef(!1),
    G = PS();
  return (
    H.useEffect(() => {
      if (l != null && l.current) {
        const j = tn(l.current),
          W = ({ x: ut, y: et }) => {
            const {
              nodeInternals: ct,
              onNodeDrag: at,
              onSelectionDrag: st,
              updateNodePositions: rt,
              nodeExtent: w,
              snapGrid: L,
              snapToGrid: S,
              nodeOrigin: R,
              onError: Z,
            } = g.getState();
            x.current = { x: ut, y: et };
            let O = !1,
              k = { x: 0, y: 0, x2: 0, y2: 0 };
            if (p.current.length > 1 && w) {
              const M = ds(p.current, R);
              k = si(M);
            }
            if (
              ((p.current = p.current.map((M) => {
                const $ = { x: ut - M.distance.x, y: et - M.distance.y };
                S &&
                  (($.x = L[0] * Math.round($.x / L[0])),
                  ($.y = L[1] * Math.round($.y / L[1])));
                const F = [
                  [w[0][0], w[0][1]],
                  [w[1][0], w[1][1]],
                ];
                p.current.length > 1 &&
                  w &&
                  !M.extent &&
                  ((F[0][0] = M.positionAbsolute.x - k.x + w[0][0]),
                  (F[1][0] =
                    M.positionAbsolute.x + (M.width ?? 0) - k.x2 + w[1][0]),
                  (F[0][1] = M.positionAbsolute.y - k.y + w[0][1]),
                  (F[1][1] =
                    M.positionAbsolute.y + (M.height ?? 0) - k.y2 + w[1][1]));
                const it = Ry(M, $, ct, F, R, Z);
                return (
                  (O =
                    O ||
                    M.position.x !== it.position.x ||
                    M.position.y !== it.position.y),
                  (M.position = it.position),
                  (M.positionAbsolute = it.positionAbsolute),
                  M
                );
              })),
              !O)
            )
              return;
            (rt(p.current, !0, !0), m(!0));
            const b = o ? at : Qo(st);
            if (b && z.current) {
              const [M, $] = Zo({
                nodeId: o,
                dragItems: p.current,
                nodeInternals: ct,
              });
              b(z.current, M, $);
            }
          },
          I = () => {
            if (!E.current) return;
            const [ut, et] = Pg(D.current, E.current);
            if (ut !== 0 || et !== 0) {
              const { transform: ct, panBy: at } = g.getState();
              ((x.current.x = (x.current.x ?? 0) - ut / ct[2]),
                (x.current.y = (x.current.y ?? 0) - et / ct[2]),
                at({ x: ut, y: et }) && W(x.current));
            }
            _.current = requestAnimationFrame(I);
          },
          Q = (ut) => {
            var R;
            const {
              nodeInternals: et,
              multiSelectionActive: ct,
              nodesDraggable: at,
              unselectNodesAndEdges: st,
              onNodeDragStart: rt,
              onSelectionDragStart: w,
            } = g.getState();
            N.current = !0;
            const L = o ? rt : Qo(w);
            ((!d || !f) &&
              !ct &&
              o &&
              (((R = et.get(o)) != null && R.selected) || st()),
              o && f && d && sf({ id: o, store: g, nodeRef: l }));
            const S = G(ut);
            if (
              ((x.current = S), (p.current = FS(et, at, S, o)), L && p.current)
            ) {
              const [Z, O] = Zo({
                nodeId: o,
                dragItems: p.current,
                nodeInternals: et,
              });
              L(ut.sourceEvent, Z, O);
            }
          };
        if (u) j.on(".drag", null);
        else {
          const ut = fb()
            .on("start", (et) => {
              const { domNode: ct, nodeDragThreshold: at } = g.getState();
              (at === 0 && Q(et), (Y.current = !1));
              const st = G(et);
              ((x.current = st),
                (E.current =
                  (ct == null ? void 0 : ct.getBoundingClientRect()) || null),
                (D.current = Cl(et.sourceEvent, E.current)));
            })
            .on("drag", (et) => {
              var rt, w;
              const ct = G(et),
                { autoPanOnNodeDrag: at, nodeDragThreshold: st } = g.getState();
              if (
                (et.sourceEvent.type === "touchmove" &&
                  et.sourceEvent.touches.length > 1 &&
                  (Y.current = !0),
                !Y.current)
              ) {
                if (
                  (!B.current && N.current && at && ((B.current = !0), I()),
                  !N.current)
                ) {
                  const L =
                      ct.xSnapped -
                      (((rt = x == null ? void 0 : x.current) == null
                        ? void 0
                        : rt.x) ?? 0),
                    S =
                      ct.ySnapped -
                      (((w = x == null ? void 0 : x.current) == null
                        ? void 0
                        : w.y) ?? 0);
                  Math.sqrt(L * L + S * S) > st && Q(et);
                }
                (x.current.x !== ct.xSnapped || x.current.y !== ct.ySnapped) &&
                  p.current &&
                  N.current &&
                  ((z.current = et.sourceEvent),
                  (D.current = Cl(et.sourceEvent, E.current)),
                  W(ct));
              }
            })
            .on("end", (et) => {
              if (
                !(!N.current || Y.current) &&
                (m(!1),
                (B.current = !1),
                (N.current = !1),
                cancelAnimationFrame(_.current),
                p.current)
              ) {
                const {
                    updateNodePositions: ct,
                    nodeInternals: at,
                    onNodeDragStop: st,
                    onSelectionDragStop: rt,
                  } = g.getState(),
                  w = o ? st : Qo(rt);
                if ((ct(p.current, !1, !1), w)) {
                  const [L, S] = Zo({
                    nodeId: o,
                    dragItems: p.current,
                    nodeInternals: at,
                  });
                  w(et.sourceEvent, L, S);
                }
              }
            })
            .filter((et) => {
              const ct = et.target;
              return (
                !et.button &&
                (!i || !sg(ct, `.${i}`, l)) &&
                (!s || sg(ct, s, l))
              );
            });
          return (
            j.call(ut),
            () => {
              j.on(".drag", null);
            }
          );
        }
      }
    }, [l, u, i, s, f, g, o, d, G]),
    y
  );
}
function jy() {
  const l = fe();
  return H.useCallback((i) => {
    const {
        nodeInternals: s,
        nodeExtent: o,
        updateNodePositions: f,
        getNodes: d,
        snapToGrid: g,
        snapGrid: y,
        onError: m,
        nodesDraggable: p,
      } = l.getState(),
      x = d().filter(
        (Y) => Y.selected && (Y.draggable || (p && typeof Y.draggable > "u")),
      ),
      _ = g ? y[0] : 5,
      E = g ? y[1] : 5,
      D = i.isShiftPressed ? 4 : 1,
      z = i.x * _ * D,
      B = i.y * E * D,
      N = x.map((Y) => {
        if (Y.positionAbsolute) {
          const G = {
            x: Y.positionAbsolute.x + z,
            y: Y.positionAbsolute.y + B,
          };
          g &&
            ((G.x = y[0] * Math.round(G.x / y[0])),
            (G.y = y[1] * Math.round(G.y / y[1])));
          const { positionAbsolute: j, position: W } = Ry(
            Y,
            G,
            s,
            o,
            void 0,
            m,
          );
          ((Y.position = W), (Y.positionAbsolute = j));
        }
        return Y;
      });
    f(N, !0, !1);
  }, []);
}
const Ka = {
  ArrowUp: { x: 0, y: -1 },
  ArrowDown: { x: 0, y: 1 },
  ArrowLeft: { x: -1, y: 0 },
  ArrowRight: { x: 1, y: 0 },
};
var Pu = (l) => {
  const u = ({
    id: i,
    type: s,
    data: o,
    xPos: f,
    yPos: d,
    xPosOrigin: g,
    yPosOrigin: y,
    selected: m,
    onClick: p,
    onMouseEnter: x,
    onMouseMove: _,
    onMouseLeave: E,
    onContextMenu: D,
    onDoubleClick: z,
    style: B,
    className: N,
    isDraggable: Y,
    isSelectable: G,
    isConnectable: j,
    isFocusable: W,
    selectNodesOnDrag: I,
    sourcePosition: Q,
    targetPosition: ut,
    hidden: et,
    resizeObserver: ct,
    dragHandle: at,
    zIndex: st,
    isParent: rt,
    noDragClassName: w,
    noPanClassName: L,
    initialized: S,
    disableKeyboardA11y: R,
    ariaLabel: Z,
    rfId: O,
    hasHandleBounds: k,
  }) => {
    const b = fe(),
      M = H.useRef(null),
      $ = H.useRef(null),
      F = H.useRef(Q),
      it = H.useRef(ut),
      ot = H.useRef(s),
      dt = G || Y || p || x || _ || E,
      tt = jy(),
      ft = Iu(i, b.getState, x),
      ht = Iu(i, b.getState, _),
      bt = Iu(i, b.getState, E),
      Nt = Iu(i, b.getState, D),
      ne = Iu(i, b.getState, z),
      Vt = (Dt) => {
        const { nodeDragThreshold: gt } = b.getState();
        if (
          (G && (!I || !Y || gt > 0) && sf({ id: i, store: b, nodeRef: M }), p)
        ) {
          const It = b.getState().nodeInternals.get(i);
          It && p(Dt, { ...It });
        }
      },
      Lt = (Dt) => {
        if (!nf(Dt) && !R)
          if (ly.includes(Dt.key) && G) {
            const gt = Dt.key === "Escape";
            sf({ id: i, store: b, unselect: gt, nodeRef: M });
          } else
            Y &&
              m &&
              Object.prototype.hasOwnProperty.call(Ka, Dt.key) &&
              (b.setState({
                ariaLiveMessage: `Moved selected node ${Dt.key.replace("Arrow", "").toLowerCase()}. New position, x: ${~~f}, y: ${~~d}`,
              }),
              tt({
                x: Ka[Dt.key].x,
                y: Ka[Dt.key].y,
                isShiftPressed: Dt.shiftKey,
              }));
      };
    (H.useEffect(
      () => () => {
        $.current &&
          (ct == null || ct.unobserve($.current), ($.current = null));
      },
      [],
    ),
      H.useEffect(() => {
        if (M.current && !et) {
          const Dt = M.current;
          (!S || !k || $.current !== Dt) &&
            ($.current && (ct == null || ct.unobserve($.current)),
            ct == null || ct.observe(Dt),
            ($.current = Dt));
        }
      }, [et, S, k]),
      H.useEffect(() => {
        const Dt = ot.current !== s,
          gt = F.current !== Q,
          It = it.current !== ut;
        M.current &&
          (Dt || gt || It) &&
          (Dt && (ot.current = s),
          gt && (F.current = Q),
          It && (it.current = ut),
          b
            .getState()
            .updateNodeDimensions([
              { id: i, nodeElement: M.current, forceUpdate: !0 },
            ]));
      }, [i, s, Q, ut]));
    const se = Hy({
      nodeRef: M,
      disabled: et || !Y,
      noDragClassName: w,
      handleSelector: at,
      nodeId: i,
      isSelectable: G,
      selectNodesOnDrag: I,
    });
    return et
      ? null
      : J.createElement(
          "div",
          {
            className: be([
              "react-flow__node",
              `react-flow__node-${s}`,
              { [L]: Y },
              N,
              { selected: m, selectable: G, parent: rt, dragging: se },
            ]),
            ref: M,
            style: {
              zIndex: st,
              transform: `translate(${g}px,${y}px)`,
              pointerEvents: dt ? "all" : "none",
              visibility: S ? "visible" : "hidden",
              ...B,
            },
            "data-id": i,
            "data-testid": `rf__node-${i}`,
            onMouseEnter: ft,
            onMouseMove: ht,
            onMouseLeave: bt,
            onContextMenu: Nt,
            onClick: Vt,
            onDoubleClick: ne,
            onKeyDown: W ? Lt : void 0,
            tabIndex: W ? 0 : void 0,
            role: W ? "button" : void 0,
            "aria-describedby": R ? void 0 : `${wy}-${O}`,
            "aria-label": Z,
          },
          J.createElement(
            mS,
            { value: i },
            J.createElement(l, {
              id: i,
              data: o,
              type: s,
              xPos: f,
              yPos: d,
              selected: m,
              isConnectable: j,
              sourcePosition: Q,
              targetPosition: ut,
              dragging: se,
              dragHandle: at,
              zIndex: st,
            }),
          ),
        );
  };
  return ((u.displayName = "NodeWrapper"), H.memo(u));
};
const tE = (l) => {
  const u = l.getNodes().filter((i) => i.selected);
  return {
    ...ds(u, l.nodeOrigin),
    transformString: `translate(${l.transform[0]}px,${l.transform[1]}px) scale(${l.transform[2]})`,
    userSelectionActive: l.userSelectionActive,
  };
};
function eE({
  onSelectionContextMenu: l,
  noPanClassName: u,
  disableKeyboardA11y: i,
}) {
  const s = fe(),
    {
      width: o,
      height: f,
      x: d,
      y: g,
      transformString: y,
      userSelectionActive: m,
    } = Gt(tE, me),
    p = jy(),
    x = H.useRef(null);
  if (
    (H.useEffect(() => {
      var D;
      i || (D = x.current) == null || D.focus({ preventScroll: !0 });
    }, [i]),
    Hy({ nodeRef: x }),
    m || !o || !f)
  )
    return null;
  const _ = l
      ? (D) => {
          const z = s
            .getState()
            .getNodes()
            .filter((B) => B.selected);
          l(D, z);
        }
      : void 0,
    E = (D) => {
      Object.prototype.hasOwnProperty.call(Ka, D.key) &&
        p({ x: Ka[D.key].x, y: Ka[D.key].y, isShiftPressed: D.shiftKey });
    };
  return J.createElement(
    "div",
    {
      className: be(["react-flow__nodesselection", "react-flow__container", u]),
      style: { transform: y },
    },
    J.createElement("div", {
      ref: x,
      className: "react-flow__nodesselection-rect",
      onContextMenu: _,
      tabIndex: i ? void 0 : -1,
      onKeyDown: i ? void 0 : E,
      style: { width: o, height: f, top: g, left: d },
    }),
  );
}
var nE = H.memo(eE);
const lE = (l) => l.nodesSelectionActive,
  Uy = ({
    children: l,
    onPaneClick: u,
    onPaneMouseEnter: i,
    onPaneMouseMove: s,
    onPaneMouseLeave: o,
    onPaneContextMenu: f,
    onPaneScroll: d,
    deleteKeyCode: g,
    onMove: y,
    onMoveStart: m,
    onMoveEnd: p,
    selectionKeyCode: x,
    selectionOnDrag: _,
    selectionMode: E,
    onSelectionStart: D,
    onSelectionEnd: z,
    multiSelectionKeyCode: B,
    panActivationKeyCode: N,
    zoomActivationKeyCode: Y,
    elementsSelectable: G,
    zoomOnScroll: j,
    zoomOnPinch: W,
    panOnScroll: I,
    panOnScrollSpeed: Q,
    panOnScrollMode: ut,
    zoomOnDoubleClick: et,
    panOnDrag: ct,
    defaultViewport: at,
    translateExtent: st,
    minZoom: rt,
    maxZoom: w,
    preventScrolling: L,
    onSelectionContextMenu: S,
    noWheelClassName: R,
    noPanClassName: Z,
    disableKeyboardA11y: O,
  }) => {
    const k = Gt(lE),
      b = oi(x),
      M = oi(N),
      $ = M || ct,
      F = M || I,
      it = b || (_ && $ !== !0);
    return (
      LS({ deleteKeyCode: g, multiSelectionKeyCode: B }),
      J.createElement(
        kS,
        {
          onMove: y,
          onMoveStart: m,
          onMoveEnd: p,
          onPaneContextMenu: f,
          elementsSelectable: G,
          zoomOnScroll: j,
          zoomOnPinch: W,
          panOnScroll: F,
          panOnScrollSpeed: Q,
          panOnScrollMode: ut,
          zoomOnDoubleClick: et,
          panOnDrag: !b && $,
          defaultViewport: at,
          translateExtent: st,
          minZoom: rt,
          maxZoom: w,
          zoomActivationKeyCode: Y,
          preventScrolling: L,
          noWheelClassName: R,
          noPanClassName: Z,
        },
        J.createElement(
          Dy,
          {
            onSelectionStart: D,
            onSelectionEnd: z,
            onPaneClick: u,
            onPaneMouseEnter: i,
            onPaneMouseMove: s,
            onPaneMouseLeave: o,
            onPaneContextMenu: f,
            onPaneScroll: d,
            panOnDrag: $,
            isSelecting: !!it,
            selectionMode: E,
          },
          l,
          k &&
            J.createElement(nE, {
              onSelectionContextMenu: S,
              noPanClassName: Z,
              disableKeyboardA11y: O,
            }),
        ),
      )
    );
  };
Uy.displayName = "FlowRenderer";
var aE = H.memo(Uy);
function uE(l) {
  return Gt(
    H.useCallback(
      (i) =>
        l
          ? fy(
              i.nodeInternals,
              { x: 0, y: 0, width: i.width, height: i.height },
              i.transform,
              !0,
            )
          : i.getNodes(),
      [l],
    ),
  );
}
function iE(l) {
  const u = {
      input: Pu(l.input || by),
      default: Pu(l.default || cf),
      output: Pu(l.output || Ey),
      group: Pu(l.group || Nf),
    },
    i = {},
    s = Object.keys(l)
      .filter((o) => !["input", "default", "output", "group"].includes(o))
      .reduce((o, f) => ((o[f] = Pu(l[f] || cf)), o), i);
  return { ...u, ...s };
}
const cE = ({ x: l, y: u, width: i, height: s, origin: o }) =>
    !i || !s
      ? { x: l, y: u }
      : o[0] < 0 || o[1] < 0 || o[0] > 1 || o[1] > 1
        ? { x: l, y: u }
        : { x: l - i * o[0], y: u - s * o[1] },
  sE = (l) => ({
    nodesDraggable: l.nodesDraggable,
    nodesConnectable: l.nodesConnectable,
    nodesFocusable: l.nodesFocusable,
    elementsSelectable: l.elementsSelectable,
    updateNodeDimensions: l.updateNodeDimensions,
    onError: l.onError,
  }),
  By = (l) => {
    const {
        nodesDraggable: u,
        nodesConnectable: i,
        nodesFocusable: s,
        elementsSelectable: o,
        updateNodeDimensions: f,
        onError: d,
      } = Gt(sE, me),
      g = uE(l.onlyRenderVisibleElements),
      y = H.useRef(),
      m = H.useMemo(() => {
        if (typeof ResizeObserver > "u") return null;
        const p = new ResizeObserver((x) => {
          const _ = x.map((E) => ({
            id: E.target.getAttribute("data-id"),
            nodeElement: E.target,
            forceUpdate: !0,
          }));
          f(_);
        });
        return ((y.current = p), p);
      }, []);
    return (
      H.useEffect(
        () => () => {
          var p;
          (p = y == null ? void 0 : y.current) == null || p.disconnect();
        },
        [],
      ),
      J.createElement(
        "div",
        { className: "react-flow__nodes", style: Tf },
        g.map((p) => {
          var W, I, Q;
          let x = p.type || "default";
          l.nodeTypes[x] ||
            (d == null || d("003", Kn.error003(x)), (x = "default"));
          const _ = l.nodeTypes[x] || l.nodeTypes.default,
            E = !!(p.draggable || (u && typeof p.draggable > "u")),
            D = !!(p.selectable || (o && typeof p.selectable > "u")),
            z = !!(p.connectable || (i && typeof p.connectable > "u")),
            B = !!(p.focusable || (s && typeof p.focusable > "u")),
            N = l.nodeExtent
              ? xf(p.positionAbsolute, l.nodeExtent)
              : p.positionAbsolute,
            Y = (N == null ? void 0 : N.x) ?? 0,
            G = (N == null ? void 0 : N.y) ?? 0,
            j = cE({
              x: Y,
              y: G,
              width: p.width ?? 0,
              height: p.height ?? 0,
              origin: l.nodeOrigin,
            });
          return J.createElement(_, {
            key: p.id,
            id: p.id,
            className: p.className,
            style: p.style,
            type: x,
            data: p.data,
            sourcePosition: p.sourcePosition || pt.Bottom,
            targetPosition: p.targetPosition || pt.Top,
            hidden: p.hidden,
            xPos: Y,
            yPos: G,
            xPosOrigin: j.x,
            yPosOrigin: j.y,
            selectNodesOnDrag: l.selectNodesOnDrag,
            onClick: l.onNodeClick,
            onMouseEnter: l.onNodeMouseEnter,
            onMouseMove: l.onNodeMouseMove,
            onMouseLeave: l.onNodeMouseLeave,
            onContextMenu: l.onNodeContextMenu,
            onDoubleClick: l.onNodeDoubleClick,
            selected: !!p.selected,
            isDraggable: E,
            isSelectable: D,
            isConnectable: z,
            isFocusable: B,
            resizeObserver: m,
            dragHandle: p.dragHandle,
            zIndex: ((W = p[Ft]) == null ? void 0 : W.z) ?? 0,
            isParent: !!((I = p[Ft]) != null && I.isParent),
            noDragClassName: l.noDragClassName,
            noPanClassName: l.noPanClassName,
            initialized: !!p.width && !!p.height,
            rfId: l.rfId,
            disableKeyboardA11y: l.disableKeyboardA11y,
            ariaLabel: p.ariaLabel,
            hasHandleBounds: !!((Q = p[Ft]) != null && Q.handleBounds),
          });
        }),
      )
    );
  };
By.displayName = "NodeRenderer";
var rE = H.memo(By);
const oE = (l, u, i) => (i === pt.Left ? l - u : i === pt.Right ? l + u : l),
  fE = (l, u, i) => (i === pt.Top ? l - u : i === pt.Bottom ? l + u : l),
  og = "react-flow__edgeupdater",
  fg = ({
    position: l,
    centerX: u,
    centerY: i,
    radius: s = 10,
    onMouseDown: o,
    onMouseEnter: f,
    onMouseOut: d,
    type: g,
  }) =>
    J.createElement("circle", {
      onMouseDown: o,
      onMouseEnter: f,
      onMouseOut: d,
      className: be([og, `${og}-${g}`]),
      cx: oE(u, s, l),
      cy: fE(i, s, l),
      r: s,
      stroke: "transparent",
      fill: "transparent",
    }),
  dE = () => !0;
var Ga = (l) => {
  const u = ({
    id: i,
    className: s,
    type: o,
    data: f,
    onClick: d,
    onEdgeDoubleClick: g,
    selected: y,
    animated: m,
    label: p,
    labelStyle: x,
    labelShowBg: _,
    labelBgStyle: E,
    labelBgPadding: D,
    labelBgBorderRadius: z,
    style: B,
    source: N,
    target: Y,
    sourceX: G,
    sourceY: j,
    targetX: W,
    targetY: I,
    sourcePosition: Q,
    targetPosition: ut,
    elementsSelectable: et,
    hidden: ct,
    sourceHandleId: at,
    targetHandleId: st,
    onContextMenu: rt,
    onMouseEnter: w,
    onMouseMove: L,
    onMouseLeave: S,
    reconnectRadius: R,
    onReconnect: Z,
    onReconnectStart: O,
    onReconnectEnd: k,
    markerEnd: b,
    markerStart: M,
    rfId: $,
    ariaLabel: F,
    isFocusable: it,
    isReconnectable: ot,
    pathOptions: dt,
    interactionWidth: tt,
    disableKeyboardA11y: ft,
  }) => {
    const ht = H.useRef(null),
      [bt, Nt] = H.useState(!1),
      [ne, Vt] = H.useState(!1),
      Lt = fe(),
      se = H.useMemo(() => `url('#${af(M, $)}')`, [M, $]),
      Dt = H.useMemo(() => `url('#${af(b, $)}')`, [b, $]);
    if (ct) return null;
    const gt = (de) => {
        var Ge;
        const {
            edges: Re,
            addSelectedEdges: He,
            unselectNodesAndEdges: Pt,
            multiSelectionActive: Se,
          } = Lt.getState(),
          $t = Re.find((Dl) => Dl.id === i);
        $t &&
          (et &&
            (Lt.setState({ nodesSelectionActive: !1 }),
            $t.selected && Se
              ? (Pt({ nodes: [], edges: [$t] }),
                (Ge = ht.current) == null || Ge.blur())
              : He([i])),
          d && d(de, $t));
      },
      It = Fu(i, Lt.getState, g),
      Oe = Fu(i, Lt.getState, rt),
      Sn = Fu(i, Lt.getState, w),
      $n = Fu(i, Lt.getState, L),
      Jn = Fu(i, Lt.getState, S),
      Kt = (de, Re) => {
        if (de.button !== 0) return;
        const { edges: He, isValidConnection: Pt } = Lt.getState(),
          Se = Re ? Y : N,
          $t = (Re ? st : at) || null,
          Ge = Re ? "target" : "source",
          Dl = Pt || dE,
          Pa = Re,
          hn = He.find((_e) => _e.id === i);
        (Vt(!0), O == null || O(de, hn, Ge));
        const Pn = (_e) => {
          (Vt(!1), k == null || k(_e, hn, Ge));
        };
        yy({
          event: de,
          handleId: $t,
          nodeId: Se,
          onConnect: (_e) => (Z == null ? void 0 : Z(hn, _e)),
          isTarget: Pa,
          getState: Lt.getState,
          setState: Lt.setState,
          isValidConnection: Dl,
          edgeUpdaterType: Ge,
          onReconnectEnd: Pn,
        });
      },
      Wn = (de) => Kt(de, !0),
      fn = (de) => Kt(de, !1),
      dn = () => Nt(!0),
      nn = () => Nt(!1),
      Fn = !et && !d,
      In = (de) => {
        var Re;
        if (!ft && ly.includes(de.key) && et) {
          const {
            unselectNodesAndEdges: He,
            addSelectedEdges: Pt,
            edges: Se,
          } = Lt.getState();
          de.key === "Escape"
            ? ((Re = ht.current) == null || Re.blur(),
              He({ edges: [Se.find((Ge) => Ge.id === i)] }))
            : Pt([i]);
        }
      };
    return J.createElement(
      "g",
      {
        className: be([
          "react-flow__edge",
          `react-flow__edge-${o}`,
          s,
          { selected: y, animated: m, inactive: Fn, updating: bt },
        ]),
        onClick: gt,
        onDoubleClick: It,
        onContextMenu: Oe,
        onMouseEnter: Sn,
        onMouseMove: $n,
        onMouseLeave: Jn,
        onKeyDown: it ? In : void 0,
        tabIndex: it ? 0 : void 0,
        role: it ? "button" : "img",
        "data-testid": `rf__edge-${i}`,
        "aria-label": F === null ? void 0 : F || `Edge from ${N} to ${Y}`,
        "aria-describedby": it ? `${Ny}-${$}` : void 0,
        ref: ht,
      },
      !ne &&
        J.createElement(l, {
          id: i,
          source: N,
          target: Y,
          selected: y,
          animated: m,
          label: p,
          labelStyle: x,
          labelShowBg: _,
          labelBgStyle: E,
          labelBgPadding: D,
          labelBgBorderRadius: z,
          data: f,
          style: B,
          sourceX: G,
          sourceY: j,
          targetX: W,
          targetY: I,
          sourcePosition: Q,
          targetPosition: ut,
          sourceHandleId: at,
          targetHandleId: st,
          markerStart: se,
          markerEnd: Dt,
          pathOptions: dt,
          interactionWidth: tt,
        }),
      ot &&
        J.createElement(
          J.Fragment,
          null,
          (ot === "source" || ot === !0) &&
            J.createElement(fg, {
              position: Q,
              centerX: G,
              centerY: j,
              radius: R,
              onMouseDown: Wn,
              onMouseEnter: dn,
              onMouseOut: nn,
              type: "source",
            }),
          (ot === "target" || ot === !0) &&
            J.createElement(fg, {
              position: ut,
              centerX: W,
              centerY: I,
              radius: R,
              onMouseDown: fn,
              onMouseEnter: dn,
              onMouseOut: nn,
              type: "target",
            }),
        ),
    );
  };
  return ((u.displayName = "EdgeWrapper"), H.memo(u));
};
function hE(l) {
  const u = {
      default: Ga(l.default || us),
      straight: Ga(l.bezier || Ef),
      step: Ga(l.step || Sf),
      smoothstep: Ga(l.step || fs),
      simplebezier: Ga(l.simplebezier || bf),
    },
    i = {},
    s = Object.keys(l)
      .filter((o) => !["default", "bezier"].includes(o))
      .reduce((o, f) => ((o[f] = Ga(l[f] || us)), o), i);
  return { ...u, ...s };
}
function dg(l, u, i = null) {
  const s = ((i == null ? void 0 : i.x) || 0) + u.x,
    o = ((i == null ? void 0 : i.y) || 0) + u.y,
    f = (i == null ? void 0 : i.width) || u.width,
    d = (i == null ? void 0 : i.height) || u.height;
  switch (l) {
    case pt.Top:
      return { x: s + f / 2, y: o };
    case pt.Right:
      return { x: s + f, y: o + d / 2 };
    case pt.Bottom:
      return { x: s + f / 2, y: o + d };
    case pt.Left:
      return { x: s, y: o + d / 2 };
  }
}
function hg(l, u) {
  return l
    ? l.length === 1 || !u
      ? l[0]
      : (u && l.find((i) => i.id === u)) || null
    : null;
}
const mE = (l, u, i, s, o, f) => {
  const d = dg(i, l, u),
    g = dg(f, s, o);
  return { sourceX: d.x, sourceY: d.y, targetX: g.x, targetY: g.y };
};
function gE({
  sourcePos: l,
  targetPos: u,
  sourceWidth: i,
  sourceHeight: s,
  targetWidth: o,
  targetHeight: f,
  width: d,
  height: g,
  transform: y,
}) {
  const m = {
    x: Math.min(l.x, u.x),
    y: Math.min(l.y, u.y),
    x2: Math.max(l.x + i, u.x + o),
    y2: Math.max(l.y + s, u.y + f),
  };
  (m.x === m.x2 && (m.x2 += 1), m.y === m.y2 && (m.y2 += 1));
  const p = si({
      x: (0 - y[0]) / y[2],
      y: (0 - y[1]) / y[2],
      width: d / y[2],
      height: g / y[2],
    }),
    x = Math.max(0, Math.min(p.x2, m.x2) - Math.max(p.x, m.x)),
    _ = Math.max(0, Math.min(p.y2, m.y2) - Math.max(p.y, m.y));
  return Math.ceil(x * _) > 0;
}
function mg(l) {
  var s, o, f, d, g;
  const u =
      ((s = l == null ? void 0 : l[Ft]) == null ? void 0 : s.handleBounds) ||
      null,
    i =
      u &&
      (l == null ? void 0 : l.width) &&
      (l == null ? void 0 : l.height) &&
      typeof ((o = l == null ? void 0 : l.positionAbsolute) == null
        ? void 0
        : o.x) < "u" &&
      typeof ((f = l == null ? void 0 : l.positionAbsolute) == null
        ? void 0
        : f.y) < "u";
  return [
    {
      x:
        ((d = l == null ? void 0 : l.positionAbsolute) == null
          ? void 0
          : d.x) || 0,
      y:
        ((g = l == null ? void 0 : l.positionAbsolute) == null
          ? void 0
          : g.y) || 0,
      width: (l == null ? void 0 : l.width) || 0,
      height: (l == null ? void 0 : l.height) || 0,
    },
    u,
    !!i,
  ];
}
const yE = [{ level: 0, isMaxLevel: !0, edges: [] }];
function pE(l, u, i = !1) {
  let s = -1;
  const o = l.reduce((d, g) => {
      var p, x;
      const y = en(g.zIndex);
      let m = y ? g.zIndex : 0;
      if (i) {
        const _ = u.get(g.target),
          E = u.get(g.source),
          D =
            g.selected ||
            (_ == null ? void 0 : _.selected) ||
            (E == null ? void 0 : E.selected),
          z = Math.max(
            ((p = E == null ? void 0 : E[Ft]) == null ? void 0 : p.z) || 0,
            ((x = _ == null ? void 0 : _[Ft]) == null ? void 0 : x.z) || 0,
            1e3,
          );
        m = (y ? g.zIndex : 0) + (D ? z : 0);
      }
      return (d[m] ? d[m].push(g) : (d[m] = [g]), (s = m > s ? m : s), d);
    }, {}),
    f = Object.entries(o).map(([d, g]) => {
      const y = +d;
      return { edges: g, level: y, isMaxLevel: y === s };
    });
  return f.length === 0 ? yE : f;
}
function vE(l, u, i) {
  const s = Gt(
    H.useCallback(
      (o) =>
        l
          ? o.edges.filter((f) => {
              const d = u.get(f.source),
                g = u.get(f.target);
              return (
                (d == null ? void 0 : d.width) &&
                (d == null ? void 0 : d.height) &&
                (g == null ? void 0 : g.width) &&
                (g == null ? void 0 : g.height) &&
                gE({
                  sourcePos: d.positionAbsolute || { x: 0, y: 0 },
                  targetPos: g.positionAbsolute || { x: 0, y: 0 },
                  sourceWidth: d.width,
                  sourceHeight: d.height,
                  targetWidth: g.width,
                  targetHeight: g.height,
                  width: o.width,
                  height: o.height,
                  transform: o.transform,
                })
              );
            })
          : o.edges,
      [l, u],
    ),
  );
  return pE(s, u, i);
}
const xE = ({ color: l = "none", strokeWidth: u = 1 }) =>
    J.createElement("polyline", {
      style: { stroke: l, strokeWidth: u },
      strokeLinecap: "round",
      strokeLinejoin: "round",
      fill: "none",
      points: "-5,-4 0,0 -5,4",
    }),
  bE = ({ color: l = "none", strokeWidth: u = 1 }) =>
    J.createElement("polyline", {
      style: { stroke: l, fill: l, strokeWidth: u },
      strokeLinecap: "round",
      strokeLinejoin: "round",
      points: "-5,-4 0,0 -5,4 -5,-4",
    }),
  gg = { [as.Arrow]: xE, [as.ArrowClosed]: bE };
function SE(l) {
  const u = fe();
  return H.useMemo(() => {
    var o, f;
    return Object.prototype.hasOwnProperty.call(gg, l)
      ? gg[l]
      : ((f = (o = u.getState()).onError) == null ||
          f.call(o, "009", Kn.error009(l)),
        null);
  }, [l]);
}
const EE = ({
    id: l,
    type: u,
    color: i,
    width: s = 12.5,
    height: o = 12.5,
    markerUnits: f = "strokeWidth",
    strokeWidth: d,
    orient: g = "auto-start-reverse",
  }) => {
    const y = SE(u);
    return y
      ? J.createElement(
          "marker",
          {
            className: "react-flow__arrowhead",
            id: l,
            markerWidth: `${s}`,
            markerHeight: `${o}`,
            viewBox: "-10 -10 20 20",
            markerUnits: f,
            orient: g,
            refX: "0",
            refY: "0",
          },
          J.createElement(y, { color: i, strokeWidth: d }),
        )
      : null;
  },
  _E =
    ({ defaultColor: l, rfId: u }) =>
    (i) => {
      const s = [];
      return i.edges
        .reduce(
          (o, f) => (
            [f.markerStart, f.markerEnd].forEach((d) => {
              if (d && typeof d == "object") {
                const g = af(d, u);
                s.includes(g) ||
                  (o.push({ id: g, color: d.color || l, ...d }), s.push(g));
              }
            }),
            o
          ),
          [],
        )
        .sort((o, f) => o.id.localeCompare(f.id));
    },
  Yy = ({ defaultColor: l, rfId: u }) => {
    const i = Gt(
      H.useCallback(_E({ defaultColor: l, rfId: u }), [l, u]),
      (s, o) => !(s.length !== o.length || s.some((f, d) => f.id !== o[d].id)),
    );
    return J.createElement(
      "defs",
      null,
      i.map((s) =>
        J.createElement(EE, {
          id: s.id,
          key: s.id,
          type: s.type,
          color: s.color,
          width: s.width,
          height: s.height,
          markerUnits: s.markerUnits,
          strokeWidth: s.strokeWidth,
          orient: s.orient,
        }),
      ),
    );
  };
Yy.displayName = "MarkerDefinitions";
var wE = H.memo(Yy);
const NE = (l) => ({
    nodesConnectable: l.nodesConnectable,
    edgesFocusable: l.edgesFocusable,
    edgesUpdatable: l.edgesUpdatable,
    elementsSelectable: l.elementsSelectable,
    width: l.width,
    height: l.height,
    connectionMode: l.connectionMode,
    nodeInternals: l.nodeInternals,
    onError: l.onError,
  }),
  qy = ({
    defaultMarkerColor: l,
    onlyRenderVisibleElements: u,
    elevateEdgesOnSelect: i,
    rfId: s,
    edgeTypes: o,
    noPanClassName: f,
    onEdgeContextMenu: d,
    onEdgeMouseEnter: g,
    onEdgeMouseMove: y,
    onEdgeMouseLeave: m,
    onEdgeClick: p,
    onEdgeDoubleClick: x,
    onReconnect: _,
    onReconnectStart: E,
    onReconnectEnd: D,
    reconnectRadius: z,
    children: B,
    disableKeyboardA11y: N,
  }) => {
    const {
        edgesFocusable: Y,
        edgesUpdatable: G,
        elementsSelectable: j,
        width: W,
        height: I,
        connectionMode: Q,
        nodeInternals: ut,
        onError: et,
      } = Gt(NE, me),
      ct = vE(u, ut, i);
    return W
      ? J.createElement(
          J.Fragment,
          null,
          ct.map(({ level: at, edges: st, isMaxLevel: rt }) =>
            J.createElement(
              "svg",
              {
                key: at,
                style: { zIndex: at },
                width: W,
                height: I,
                className: "react-flow__edges react-flow__container",
              },
              rt && J.createElement(wE, { defaultColor: l, rfId: s }),
              J.createElement(
                "g",
                null,
                st.map((w) => {
                  const [L, S, R] = mg(ut.get(w.source)),
                    [Z, O, k] = mg(ut.get(w.target));
                  if (!R || !k) return null;
                  let b = w.type || "default";
                  o[b] ||
                    (et == null || et("011", Kn.error011(b)), (b = "default"));
                  const M = o[b] || o.default,
                    $ =
                      Q === ua.Strict
                        ? O.target
                        : (O.target ?? []).concat(O.source ?? []),
                    F = hg(S.source, w.sourceHandle),
                    it = hg($, w.targetHandle),
                    ot = (F == null ? void 0 : F.position) || pt.Bottom,
                    dt = (it == null ? void 0 : it.position) || pt.Top,
                    tt = !!(w.focusable || (Y && typeof w.focusable > "u")),
                    ft = w.reconnectable || w.updatable,
                    ht = typeof _ < "u" && (ft || (G && typeof ft > "u"));
                  if (!F || !it)
                    return (et == null || et("008", Kn.error008(F, w)), null);
                  const {
                    sourceX: bt,
                    sourceY: Nt,
                    targetX: ne,
                    targetY: Vt,
                  } = mE(L, F, ot, Z, it, dt);
                  return J.createElement(M, {
                    key: w.id,
                    id: w.id,
                    className: be([w.className, f]),
                    type: b,
                    data: w.data,
                    selected: !!w.selected,
                    animated: !!w.animated,
                    hidden: !!w.hidden,
                    label: w.label,
                    labelStyle: w.labelStyle,
                    labelShowBg: w.labelShowBg,
                    labelBgStyle: w.labelBgStyle,
                    labelBgPadding: w.labelBgPadding,
                    labelBgBorderRadius: w.labelBgBorderRadius,
                    style: w.style,
                    source: w.source,
                    target: w.target,
                    sourceHandleId: w.sourceHandle,
                    targetHandleId: w.targetHandle,
                    markerEnd: w.markerEnd,
                    markerStart: w.markerStart,
                    sourceX: bt,
                    sourceY: Nt,
                    targetX: ne,
                    targetY: Vt,
                    sourcePosition: ot,
                    targetPosition: dt,
                    elementsSelectable: j,
                    onContextMenu: d,
                    onMouseEnter: g,
                    onMouseMove: y,
                    onMouseLeave: m,
                    onClick: p,
                    onEdgeDoubleClick: x,
                    onReconnect: _,
                    onReconnectStart: E,
                    onReconnectEnd: D,
                    reconnectRadius: z,
                    rfId: s,
                    ariaLabel: w.ariaLabel,
                    isFocusable: tt,
                    isReconnectable: ht,
                    pathOptions: "pathOptions" in w ? w.pathOptions : void 0,
                    interactionWidth: w.interactionWidth,
                    disableKeyboardA11y: N,
                  });
                }),
              ),
            ),
          ),
          B,
        )
      : null;
  };
qy.displayName = "EdgeRenderer";
var AE = H.memo(qy);
const TE = (l) =>
  `translate(${l.transform[0]}px,${l.transform[1]}px) scale(${l.transform[2]})`;
function zE({ children: l }) {
  const u = Gt(TE);
  return J.createElement(
    "div",
    {
      className: "react-flow__viewport react-flow__container",
      style: { transform: u },
    },
    l,
  );
}
function ME(l) {
  const u = Af(),
    i = H.useRef(!1);
  H.useEffect(() => {
    !i.current &&
      u.viewportInitialized &&
      l &&
      (setTimeout(() => l(u), 1), (i.current = !0));
  }, [l, u.viewportInitialized]);
}
const CE = {
    [pt.Left]: pt.Right,
    [pt.Right]: pt.Left,
    [pt.Top]: pt.Bottom,
    [pt.Bottom]: pt.Top,
  },
  Xy = ({
    nodeId: l,
    handleType: u,
    style: i,
    type: s = Ml.Bezier,
    CustomComponent: o,
    connectionStatus: f,
  }) => {
    var I, Q, ut;
    const {
        fromNode: d,
        handleId: g,
        toX: y,
        toY: m,
        connectionMode: p,
      } = Gt(
        H.useCallback(
          (et) => ({
            fromNode: et.nodeInternals.get(l),
            handleId: et.connectionHandleId,
            toX: (et.connectionPosition.x - et.transform[0]) / et.transform[2],
            toY: (et.connectionPosition.y - et.transform[1]) / et.transform[2],
            connectionMode: et.connectionMode,
          }),
          [l],
        ),
        me,
      ),
      x = (I = d == null ? void 0 : d[Ft]) == null ? void 0 : I.handleBounds;
    let _ = x == null ? void 0 : x[u];
    if (
      (p === ua.Loose &&
        (_ =
          _ || (x == null ? void 0 : x[u === "source" ? "target" : "source"])),
      !d || !_)
    )
      return null;
    const E = g ? _.find((et) => et.id === g) : _[0],
      D = E ? E.x + E.width / 2 : (d.width ?? 0) / 2,
      z = E ? E.y + E.height / 2 : (d.height ?? 0),
      B = (((Q = d.positionAbsolute) == null ? void 0 : Q.x) ?? 0) + D,
      N = (((ut = d.positionAbsolute) == null ? void 0 : ut.y) ?? 0) + z,
      Y = E == null ? void 0 : E.position,
      G = Y ? CE[Y] : null;
    if (!Y || !G) return null;
    if (o)
      return J.createElement(o, {
        connectionLineType: s,
        connectionLineStyle: i,
        fromNode: d,
        fromHandle: E,
        fromX: B,
        fromY: N,
        toX: y,
        toY: m,
        fromPosition: Y,
        toPosition: G,
        connectionStatus: f,
      });
    let j = "";
    const W = {
      sourceX: B,
      sourceY: N,
      sourcePosition: Y,
      targetX: y,
      targetY: m,
      targetPosition: G,
    };
    return (
      s === Ml.Bezier
        ? ([j] = sy(W))
        : s === Ml.Step
          ? ([j] = lf({ ...W, borderRadius: 0 }))
          : s === Ml.SmoothStep
            ? ([j] = lf(W))
            : s === Ml.SimpleBezier
              ? ([j] = cy(W))
              : (j = `M${B},${N} ${y},${m}`),
      J.createElement("path", {
        d: j,
        fill: "none",
        className: "react-flow__connection-path",
        style: i,
      })
    );
  };
Xy.displayName = "ConnectionLine";
const DE = (l) => ({
  nodeId: l.connectionNodeId,
  handleType: l.connectionHandleType,
  nodesConnectable: l.nodesConnectable,
  connectionStatus: l.connectionStatus,
  width: l.width,
  height: l.height,
});
function OE({ containerStyle: l, style: u, type: i, component: s }) {
  const {
    nodeId: o,
    handleType: f,
    nodesConnectable: d,
    width: g,
    height: y,
    connectionStatus: m,
  } = Gt(DE, me);
  return !(o && f && g && d)
    ? null
    : J.createElement(
        "svg",
        {
          style: l,
          width: g,
          height: y,
          className:
            "react-flow__edges react-flow__connectionline react-flow__container",
        },
        J.createElement(
          "g",
          { className: be(["react-flow__connection", m]) },
          J.createElement(Xy, {
            nodeId: o,
            handleType: f,
            style: u,
            type: i,
            CustomComponent: s,
            connectionStatus: m,
          }),
        ),
      );
}
function yg(l, u) {
  return (H.useRef(null), fe(), H.useMemo(() => u(l), [l]));
}
const Vy = ({
  nodeTypes: l,
  edgeTypes: u,
  onMove: i,
  onMoveStart: s,
  onMoveEnd: o,
  onInit: f,
  onNodeClick: d,
  onEdgeClick: g,
  onNodeDoubleClick: y,
  onEdgeDoubleClick: m,
  onNodeMouseEnter: p,
  onNodeMouseMove: x,
  onNodeMouseLeave: _,
  onNodeContextMenu: E,
  onSelectionContextMenu: D,
  onSelectionStart: z,
  onSelectionEnd: B,
  connectionLineType: N,
  connectionLineStyle: Y,
  connectionLineComponent: G,
  connectionLineContainerStyle: j,
  selectionKeyCode: W,
  selectionOnDrag: I,
  selectionMode: Q,
  multiSelectionKeyCode: ut,
  panActivationKeyCode: et,
  zoomActivationKeyCode: ct,
  deleteKeyCode: at,
  onlyRenderVisibleElements: st,
  elementsSelectable: rt,
  selectNodesOnDrag: w,
  defaultViewport: L,
  translateExtent: S,
  minZoom: R,
  maxZoom: Z,
  preventScrolling: O,
  defaultMarkerColor: k,
  zoomOnScroll: b,
  zoomOnPinch: M,
  panOnScroll: $,
  panOnScrollSpeed: F,
  panOnScrollMode: it,
  zoomOnDoubleClick: ot,
  panOnDrag: dt,
  onPaneClick: tt,
  onPaneMouseEnter: ft,
  onPaneMouseMove: ht,
  onPaneMouseLeave: bt,
  onPaneScroll: Nt,
  onPaneContextMenu: ne,
  onEdgeContextMenu: Vt,
  onEdgeMouseEnter: Lt,
  onEdgeMouseMove: se,
  onEdgeMouseLeave: Dt,
  onReconnect: gt,
  onReconnectStart: It,
  onReconnectEnd: Oe,
  reconnectRadius: Sn,
  noDragClassName: $n,
  noWheelClassName: Jn,
  noPanClassName: Kt,
  elevateEdgesOnSelect: Wn,
  disableKeyboardA11y: fn,
  nodeOrigin: dn,
  nodeExtent: nn,
  rfId: Fn,
}) => {
  const In = yg(l, iE),
    de = yg(u, hE);
  return (
    ME(f),
    J.createElement(
      aE,
      {
        onPaneClick: tt,
        onPaneMouseEnter: ft,
        onPaneMouseMove: ht,
        onPaneMouseLeave: bt,
        onPaneContextMenu: ne,
        onPaneScroll: Nt,
        deleteKeyCode: at,
        selectionKeyCode: W,
        selectionOnDrag: I,
        selectionMode: Q,
        onSelectionStart: z,
        onSelectionEnd: B,
        multiSelectionKeyCode: ut,
        panActivationKeyCode: et,
        zoomActivationKeyCode: ct,
        elementsSelectable: rt,
        onMove: i,
        onMoveStart: s,
        onMoveEnd: o,
        zoomOnScroll: b,
        zoomOnPinch: M,
        zoomOnDoubleClick: ot,
        panOnScroll: $,
        panOnScrollSpeed: F,
        panOnScrollMode: it,
        panOnDrag: dt,
        defaultViewport: L,
        translateExtent: S,
        minZoom: R,
        maxZoom: Z,
        onSelectionContextMenu: D,
        preventScrolling: O,
        noDragClassName: $n,
        noWheelClassName: Jn,
        noPanClassName: Kt,
        disableKeyboardA11y: fn,
      },
      J.createElement(
        zE,
        null,
        J.createElement(
          AE,
          {
            edgeTypes: de,
            onEdgeClick: g,
            onEdgeDoubleClick: m,
            onlyRenderVisibleElements: st,
            onEdgeContextMenu: Vt,
            onEdgeMouseEnter: Lt,
            onEdgeMouseMove: se,
            onEdgeMouseLeave: Dt,
            onReconnect: gt,
            onReconnectStart: It,
            onReconnectEnd: Oe,
            reconnectRadius: Sn,
            defaultMarkerColor: k,
            noPanClassName: Kt,
            elevateEdgesOnSelect: !!Wn,
            disableKeyboardA11y: fn,
            rfId: Fn,
          },
          J.createElement(OE, {
            style: Y,
            type: N,
            component: G,
            containerStyle: j,
          }),
        ),
        J.createElement("div", { className: "react-flow__edgelabel-renderer" }),
        J.createElement(rE, {
          nodeTypes: In,
          onNodeClick: d,
          onNodeDoubleClick: y,
          onNodeMouseEnter: p,
          onNodeMouseMove: x,
          onNodeMouseLeave: _,
          onNodeContextMenu: E,
          selectNodesOnDrag: w,
          onlyRenderVisibleElements: st,
          noPanClassName: Kt,
          noDragClassName: $n,
          disableKeyboardA11y: fn,
          nodeOrigin: dn,
          nodeExtent: nn,
          rfId: Fn,
        }),
      ),
    )
  );
};
Vy.displayName = "GraphView";
var RE = H.memo(Vy);
const rf = [
    [Number.NEGATIVE_INFINITY, Number.NEGATIVE_INFINITY],
    [Number.POSITIVE_INFINITY, Number.POSITIVE_INFINITY],
  ],
  Al = {
    rfId: "1",
    width: 0,
    height: 0,
    transform: [0, 0, 1],
    nodeInternals: new Map(),
    edges: [],
    onNodesChange: null,
    onEdgesChange: null,
    hasDefaultNodes: !1,
    hasDefaultEdges: !1,
    d3Zoom: null,
    d3Selection: null,
    d3ZoomHandler: void 0,
    minZoom: 0.5,
    maxZoom: 2,
    translateExtent: rf,
    nodeExtent: rf,
    nodesSelectionActive: !1,
    userSelectionActive: !1,
    userSelectionRect: null,
    connectionNodeId: null,
    connectionHandleId: null,
    connectionHandleType: "source",
    connectionPosition: { x: 0, y: 0 },
    connectionStatus: null,
    connectionMode: ua.Strict,
    domNode: null,
    paneDragging: !1,
    noPanClassName: "nopan",
    nodeOrigin: [0, 0],
    nodeDragThreshold: 0,
    snapGrid: [15, 15],
    snapToGrid: !1,
    nodesDraggable: !0,
    nodesConnectable: !0,
    nodesFocusable: !0,
    edgesFocusable: !0,
    edgesUpdatable: !0,
    elementsSelectable: !0,
    elevateNodesOnSelect: !0,
    fitViewOnInit: !1,
    fitViewOnInitDone: !1,
    fitViewOnInitOptions: void 0,
    onSelectionChange: [],
    multiSelectionActive: !1,
    connectionStartHandle: null,
    connectionEndHandle: null,
    connectionClickStartHandle: null,
    connectOnClick: !0,
    ariaLiveMessage: "",
    autoPanOnConnect: !0,
    autoPanOnNodeDrag: !0,
    connectionRadius: 20,
    onError: sS,
    isValidConnection: void 0,
  },
  HE = () =>
    _1(
      (l, u) => ({
        ...Al,
        setNodes: (i) => {
          const {
            nodeInternals: s,
            nodeOrigin: o,
            elevateNodesOnSelect: f,
          } = u();
          l({ nodeInternals: Lo(i, s, o, f) });
        },
        getNodes: () => Array.from(u().nodeInternals.values()),
        setEdges: (i) => {
          const { defaultEdgeOptions: s = {} } = u();
          l({ edges: i.map((o) => ({ ...s, ...o })) });
        },
        setDefaultNodesAndEdges: (i, s) => {
          const o = typeof i < "u",
            f = typeof s < "u",
            d = o
              ? Lo(i, new Map(), u().nodeOrigin, u().elevateNodesOnSelect)
              : new Map();
          l({
            nodeInternals: d,
            edges: f ? s : [],
            hasDefaultNodes: o,
            hasDefaultEdges: f,
          });
        },
        updateNodeDimensions: (i) => {
          const {
              onNodesChange: s,
              nodeInternals: o,
              fitViewOnInit: f,
              fitViewOnInitDone: d,
              fitViewOnInitOptions: g,
              domNode: y,
              nodeOrigin: m,
            } = u(),
            p = y == null ? void 0 : y.querySelector(".react-flow__viewport");
          if (!p) return;
          const x = window.getComputedStyle(p),
            { m22: _ } = new window.DOMMatrixReadOnly(x.transform),
            E = i.reduce((z, B) => {
              const N = o.get(B.id);
              if (N != null && N.hidden)
                o.set(N.id, { ...N, [Ft]: { ...N[Ft], handleBounds: void 0 } });
              else if (N) {
                const Y = vf(B.nodeElement);
                !!(
                  Y.width &&
                  Y.height &&
                  (N.width !== Y.width ||
                    N.height !== Y.height ||
                    B.forceUpdate)
                ) &&
                  (o.set(N.id, {
                    ...N,
                    [Ft]: {
                      ...N[Ft],
                      handleBounds: {
                        source: rg(".source", B.nodeElement, _, m),
                        target: rg(".target", B.nodeElement, _, m),
                      },
                    },
                    ...Y,
                  }),
                  z.push({ id: N.id, type: "dimensions", dimensions: Y }));
              }
              return z;
            }, []);
          Ty(o, m);
          const D = d || (f && !d && zy(u, { initial: !0, ...g }));
          (l({ nodeInternals: new Map(o), fitViewOnInitDone: D }),
            (E == null ? void 0 : E.length) > 0 && (s == null || s(E)));
        },
        updateNodePositions: (i, s = !0, o = !1) => {
          const { triggerNodeChanges: f } = u(),
            d = i.map((g) => {
              const y = { id: g.id, type: "position", dragging: o };
              return (
                s &&
                  ((y.positionAbsolute = g.positionAbsolute),
                  (y.position = g.position)),
                y
              );
            });
          f(d);
        },
        triggerNodeChanges: (i) => {
          const {
            onNodesChange: s,
            nodeInternals: o,
            hasDefaultNodes: f,
            nodeOrigin: d,
            getNodes: g,
            elevateNodesOnSelect: y,
          } = u();
          if (i != null && i.length) {
            if (f) {
              const m = Cy(i, g()),
                p = Lo(m, o, d, y);
              l({ nodeInternals: p });
            }
            s == null || s(i);
          }
        },
        addSelectedNodes: (i) => {
          const { multiSelectionActive: s, edges: o, getNodes: f } = u();
          let d,
            g = null;
          (s
            ? (d = i.map((y) => zl(y, !0)))
            : ((d = Za(f(), i)), (g = Za(o, []))),
            Qc({ changedNodes: d, changedEdges: g, get: u, set: l }));
        },
        addSelectedEdges: (i) => {
          const { multiSelectionActive: s, edges: o, getNodes: f } = u();
          let d,
            g = null;
          (s
            ? (d = i.map((y) => zl(y, !0)))
            : ((d = Za(o, i)), (g = Za(f(), []))),
            Qc({ changedNodes: g, changedEdges: d, get: u, set: l }));
        },
        unselectNodesAndEdges: ({ nodes: i, edges: s } = {}) => {
          const { edges: o, getNodes: f } = u(),
            d = i || f(),
            g = s || o,
            y = d.map((p) => ((p.selected = !1), zl(p.id, !1))),
            m = g.map((p) => zl(p.id, !1));
          Qc({ changedNodes: y, changedEdges: m, get: u, set: l });
        },
        setMinZoom: (i) => {
          const { d3Zoom: s, maxZoom: o } = u();
          (s == null || s.scaleExtent([i, o]), l({ minZoom: i }));
        },
        setMaxZoom: (i) => {
          const { d3Zoom: s, minZoom: o } = u();
          (s == null || s.scaleExtent([o, i]), l({ maxZoom: i }));
        },
        setTranslateExtent: (i) => {
          var s;
          ((s = u().d3Zoom) == null || s.translateExtent(i),
            l({ translateExtent: i }));
        },
        resetSelectedElements: () => {
          const { edges: i, getNodes: s } = u(),
            f = s()
              .filter((g) => g.selected)
              .map((g) => zl(g.id, !1)),
            d = i.filter((g) => g.selected).map((g) => zl(g.id, !1));
          Qc({ changedNodes: f, changedEdges: d, get: u, set: l });
        },
        setNodeExtent: (i) => {
          const { nodeInternals: s } = u();
          (s.forEach((o) => {
            o.positionAbsolute = xf(o.position, i);
          }),
            l({ nodeExtent: i, nodeInternals: new Map(s) }));
        },
        panBy: (i) => {
          const {
            transform: s,
            width: o,
            height: f,
            d3Zoom: d,
            d3Selection: g,
            translateExtent: y,
          } = u();
          if (!d || !g || (!i.x && !i.y)) return !1;
          const m = Qn.translate(s[0] + i.x, s[1] + i.y).scale(s[2]),
            p = [
              [0, 0],
              [o, f],
            ],
            x = d == null ? void 0 : d.constrain()(m, p, y);
          return (
            d.transform(g, x),
            s[0] !== x.x || s[1] !== x.y || s[2] !== x.k
          );
        },
        cancelConnection: () =>
          l({
            connectionNodeId: Al.connectionNodeId,
            connectionHandleId: Al.connectionHandleId,
            connectionHandleType: Al.connectionHandleType,
            connectionStatus: Al.connectionStatus,
            connectionStartHandle: Al.connectionStartHandle,
            connectionEndHandle: Al.connectionEndHandle,
          }),
        reset: () => l({ ...Al }),
      }),
      Object.is,
    ),
  zf = ({ children: l }) => {
    const u = H.useRef(null);
    return (
      u.current || (u.current = HE()),
      J.createElement(eS, { value: u.current }, l)
    );
  };
zf.displayName = "ReactFlowProvider";
const Ly = ({ children: l }) =>
  H.useContext(os)
    ? J.createElement(J.Fragment, null, l)
    : J.createElement(zf, null, l);
Ly.displayName = "ReactFlowWrapper";
const jE = { input: by, default: cf, output: Ey, group: Nf },
  UE = {
    default: us,
    straight: Ef,
    step: Sf,
    smoothstep: fs,
    simplebezier: bf,
  },
  BE = [0, 0],
  YE = [15, 15],
  qE = { x: 0, y: 0, zoom: 1 },
  XE = {
    width: "100%",
    height: "100%",
    overflow: "hidden",
    position: "relative",
    zIndex: 0,
  },
  Gy = H.forwardRef(
    (
      {
        nodes: l,
        edges: u,
        defaultNodes: i,
        defaultEdges: s,
        className: o,
        nodeTypes: f = jE,
        edgeTypes: d = UE,
        onNodeClick: g,
        onEdgeClick: y,
        onInit: m,
        onMove: p,
        onMoveStart: x,
        onMoveEnd: _,
        onConnect: E,
        onConnectStart: D,
        onConnectEnd: z,
        onClickConnectStart: B,
        onClickConnectEnd: N,
        onNodeMouseEnter: Y,
        onNodeMouseMove: G,
        onNodeMouseLeave: j,
        onNodeContextMenu: W,
        onNodeDoubleClick: I,
        onNodeDragStart: Q,
        onNodeDrag: ut,
        onNodeDragStop: et,
        onNodesDelete: ct,
        onEdgesDelete: at,
        onSelectionChange: st,
        onSelectionDragStart: rt,
        onSelectionDrag: w,
        onSelectionDragStop: L,
        onSelectionContextMenu: S,
        onSelectionStart: R,
        onSelectionEnd: Z,
        connectionMode: O = ua.Strict,
        connectionLineType: k = Ml.Bezier,
        connectionLineStyle: b,
        connectionLineComponent: M,
        connectionLineContainerStyle: $,
        deleteKeyCode: F = "Backspace",
        selectionKeyCode: it = "Shift",
        selectionOnDrag: ot = !1,
        selectionMode: dt = ri.Full,
        panActivationKeyCode: tt = "Space",
        multiSelectionKeyCode: ft = ls() ? "Meta" : "Control",
        zoomActivationKeyCode: ht = ls() ? "Meta" : "Control",
        snapToGrid: bt = !1,
        snapGrid: Nt = YE,
        onlyRenderVisibleElements: ne = !1,
        selectNodesOnDrag: Vt = !0,
        nodesDraggable: Lt,
        nodesConnectable: se,
        nodesFocusable: Dt,
        nodeOrigin: gt = BE,
        edgesFocusable: It,
        edgesUpdatable: Oe,
        elementsSelectable: Sn,
        defaultViewport: $n = qE,
        minZoom: Jn = 0.5,
        maxZoom: Kt = 2,
        translateExtent: Wn = rf,
        preventScrolling: fn = !0,
        nodeExtent: dn,
        defaultMarkerColor: nn = "#b1b1b7",
        zoomOnScroll: Fn = !0,
        zoomOnPinch: In = !0,
        panOnScroll: de = !1,
        panOnScrollSpeed: Re = 0.5,
        panOnScrollMode: He = ea.Free,
        zoomOnDoubleClick: Pt = !0,
        panOnDrag: Se = !0,
        onPaneClick: $t,
        onPaneMouseEnter: Ge,
        onPaneMouseMove: Dl,
        onPaneMouseLeave: Pa,
        onPaneScroll: hn,
        onPaneContextMenu: Pn,
        children: Ol,
        onEdgeContextMenu: _e,
        onEdgeDoubleClick: ia,
        onEdgeMouseEnter: Rl,
        onEdgeMouseMove: ms,
        onEdgeMouseLeave: gi,
        onEdgeUpdate: tu,
        onEdgeUpdateStart: Hl,
        onEdgeUpdateEnd: gs,
        onReconnect: yi,
        onReconnectStart: pi,
        onReconnectEnd: vi,
        reconnectRadius: eu = 10,
        edgeUpdaterRadius: nu = 10,
        onNodesChange: xi,
        onEdgesChange: bi,
        noDragClassName: mn = "nodrag",
        noWheelClassName: re = "nowheel",
        noPanClassName: ge = "nopan",
        fitView: tl = !1,
        fitViewOptions: lu,
        connectOnClick: ys = !0,
        attributionPosition: ps,
        proOptions: Si,
        defaultEdgeOptions: jl,
        elevateNodesOnSelect: au = !0,
        elevateEdgesOnSelect: el = !1,
        disableKeyboardA11y: En = !1,
        autoPanOnConnect: Ul = !0,
        autoPanOnNodeDrag: nl = !0,
        connectionRadius: le = 20,
        isValidConnection: Ei,
        onError: _i,
        style: _n,
        id: wn,
        nodeDragThreshold: vs,
        ...wi
      },
      Ni,
    ) => {
      const uu = wn || "1";
      return J.createElement(
        "div",
        {
          ...wi,
          style: { ..._n, ...XE },
          ref: Ni,
          className: be(["react-flow", o]),
          "data-testid": "rf__wrapper",
          id: wn,
        },
        J.createElement(
          Ly,
          null,
          J.createElement(RE, {
            onInit: m,
            onMove: p,
            onMoveStart: x,
            onMoveEnd: _,
            onNodeClick: g,
            onEdgeClick: y,
            onNodeMouseEnter: Y,
            onNodeMouseMove: G,
            onNodeMouseLeave: j,
            onNodeContextMenu: W,
            onNodeDoubleClick: I,
            nodeTypes: f,
            edgeTypes: d,
            connectionLineType: k,
            connectionLineStyle: b,
            connectionLineComponent: M,
            connectionLineContainerStyle: $,
            selectionKeyCode: it,
            selectionOnDrag: ot,
            selectionMode: dt,
            deleteKeyCode: F,
            multiSelectionKeyCode: ft,
            panActivationKeyCode: tt,
            zoomActivationKeyCode: ht,
            onlyRenderVisibleElements: ne,
            selectNodesOnDrag: Vt,
            defaultViewport: $n,
            translateExtent: Wn,
            minZoom: Jn,
            maxZoom: Kt,
            preventScrolling: fn,
            zoomOnScroll: Fn,
            zoomOnPinch: In,
            zoomOnDoubleClick: Pt,
            panOnScroll: de,
            panOnScrollSpeed: Re,
            panOnScrollMode: He,
            panOnDrag: Se,
            onPaneClick: $t,
            onPaneMouseEnter: Ge,
            onPaneMouseMove: Dl,
            onPaneMouseLeave: Pa,
            onPaneScroll: hn,
            onPaneContextMenu: Pn,
            onSelectionContextMenu: S,
            onSelectionStart: R,
            onSelectionEnd: Z,
            onEdgeContextMenu: _e,
            onEdgeDoubleClick: ia,
            onEdgeMouseEnter: Rl,
            onEdgeMouseMove: ms,
            onEdgeMouseLeave: gi,
            onReconnect: yi ?? tu,
            onReconnectStart: pi ?? Hl,
            onReconnectEnd: vi ?? gs,
            reconnectRadius: eu ?? nu,
            defaultMarkerColor: nn,
            noDragClassName: mn,
            noWheelClassName: re,
            noPanClassName: ge,
            elevateEdgesOnSelect: el,
            rfId: uu,
            disableKeyboardA11y: En,
            nodeOrigin: gt,
            nodeExtent: dn,
          }),
          J.createElement(CS, {
            nodes: l,
            edges: u,
            defaultNodes: i,
            defaultEdges: s,
            onConnect: E,
            onConnectStart: D,
            onConnectEnd: z,
            onClickConnectStart: B,
            onClickConnectEnd: N,
            nodesDraggable: Lt,
            nodesConnectable: se,
            nodesFocusable: Dt,
            edgesFocusable: It,
            edgesUpdatable: Oe,
            elementsSelectable: Sn,
            elevateNodesOnSelect: au,
            minZoom: Jn,
            maxZoom: Kt,
            nodeExtent: dn,
            onNodesChange: xi,
            onEdgesChange: bi,
            snapToGrid: bt,
            snapGrid: Nt,
            connectionMode: O,
            translateExtent: Wn,
            connectOnClick: ys,
            defaultEdgeOptions: jl,
            fitView: tl,
            fitViewOptions: lu,
            onNodesDelete: ct,
            onEdgesDelete: at,
            onNodeDragStart: Q,
            onNodeDrag: ut,
            onNodeDragStop: et,
            onSelectionDrag: w,
            onSelectionDragStart: rt,
            onSelectionDragStop: L,
            noPanClassName: ge,
            nodeOrigin: gt,
            rfId: uu,
            autoPanOnConnect: Ul,
            autoPanOnNodeDrag: nl,
            onError: _i,
            connectionRadius: le,
            isValidConnection: Ei,
            nodeDragThreshold: vs,
          }),
          J.createElement(zS, { onSelectionChange: st }),
          Ol,
          J.createElement(lS, { proOptions: Si, position: ps }),
          J.createElement(jS, { rfId: uu, disableKeyboardA11y: En }),
        ),
      );
    },
  );
Gy.displayName = "ReactFlow";
function Zy(l) {
  return (u) => {
    const [i, s] = H.useState(u),
      o = H.useCallback((f) => s((d) => l(f, d)), []);
    return [i, s, o];
  };
}
const VE = Zy(Cy),
  LE = Zy(JS),
  Qy = ({
    id: l,
    x: u,
    y: i,
    width: s,
    height: o,
    style: f,
    color: d,
    strokeColor: g,
    strokeWidth: y,
    className: m,
    borderRadius: p,
    shapeRendering: x,
    onClick: _,
    selected: E,
  }) => {
    const { background: D, backgroundColor: z } = f || {},
      B = d || D || z;
    return J.createElement("rect", {
      className: be(["react-flow__minimap-node", { selected: E }, m]),
      x: u,
      y: i,
      rx: p,
      ry: p,
      width: s,
      height: o,
      fill: B,
      stroke: g,
      strokeWidth: y,
      shapeRendering: x,
      onClick: _ ? (N) => _(N, l) : void 0,
    });
  };
Qy.displayName = "MiniMapNode";
var GE = H.memo(Qy);
const ZE = (l) => l.nodeOrigin,
  QE = (l) => l.getNodes().filter((u) => !u.hidden && u.width && u.height),
  ko = (l) => (l instanceof Function ? l : () => l);
function kE({
  nodeStrokeColor: l = "transparent",
  nodeColor: u = "#e2e2e2",
  nodeClassName: i = "",
  nodeBorderRadius: s = 5,
  nodeStrokeWidth: o = 2,
  nodeComponent: f = GE,
  onClick: d,
}) {
  const g = Gt(QE, me),
    y = Gt(ZE),
    m = ko(u),
    p = ko(l),
    x = ko(i),
    _ =
      typeof window > "u" || window.chrome
        ? "crispEdges"
        : "geometricPrecision";
  return J.createElement(
    J.Fragment,
    null,
    g.map((E) => {
      const { x: D, y: z } = la(E, y).positionAbsolute;
      return J.createElement(f, {
        key: E.id,
        x: D,
        y: z,
        width: E.width,
        height: E.height,
        style: E.style,
        selected: E.selected,
        className: x(E),
        color: m(E),
        borderRadius: s,
        strokeColor: p(E),
        strokeWidth: o,
        shapeRendering: _,
        onClick: d,
        id: E.id,
      });
    }),
  );
}
var KE = H.memo(kE);
const $E = 200,
  JE = 150,
  WE = (l) => {
    const u = l.getNodes(),
      i = {
        x: -l.transform[0] / l.transform[2],
        y: -l.transform[1] / l.transform[2],
        width: l.width / l.transform[2],
        height: l.height / l.transform[2],
      };
    return {
      viewBB: i,
      boundingRect: u.length > 0 ? iS(ds(u, l.nodeOrigin), i) : i,
      rfId: l.rfId,
    };
  },
  FE = "react-flow__minimap-desc";
function ky({
  style: l,
  className: u,
  nodeStrokeColor: i = "transparent",
  nodeColor: s = "#e2e2e2",
  nodeClassName: o = "",
  nodeBorderRadius: f = 5,
  nodeStrokeWidth: d = 2,
  nodeComponent: g,
  maskColor: y = "rgb(240, 240, 240, 0.6)",
  maskStrokeColor: m = "none",
  maskStrokeWidth: p = 1,
  position: x = "bottom-right",
  onClick: _,
  onNodeClick: E,
  pannable: D = !1,
  zoomable: z = !1,
  ariaLabel: B = "React Flow mini map",
  inversePan: N = !1,
  zoomStep: Y = 10,
  offsetScale: G = 5,
}) {
  const j = fe(),
    W = H.useRef(null),
    { boundingRect: I, viewBB: Q, rfId: ut } = Gt(WE, me),
    et = (l == null ? void 0 : l.width) ?? $E,
    ct = (l == null ? void 0 : l.height) ?? JE,
    at = I.width / et,
    st = I.height / ct,
    rt = Math.max(at, st),
    w = rt * et,
    L = rt * ct,
    S = G * rt,
    R = I.x - (w - I.width) / 2 - S,
    Z = I.y - (L - I.height) / 2 - S,
    O = w + S * 2,
    k = L + S * 2,
    b = `${FE}-${ut}`,
    M = H.useRef(0);
  ((M.current = rt),
    H.useEffect(() => {
      if (W.current) {
        const it = tn(W.current),
          ot = (ft) => {
            const { transform: ht, d3Selection: bt, d3Zoom: Nt } = j.getState();
            if (ft.sourceEvent.type !== "wheel" || !bt || !Nt) return;
            const ne =
                -ft.sourceEvent.deltaY *
                (ft.sourceEvent.deltaMode === 1
                  ? 0.05
                  : ft.sourceEvent.deltaMode
                    ? 1
                    : 0.002) *
                Y,
              Vt = ht[2] * Math.pow(2, ne);
            Nt.scaleTo(bt, Vt);
          },
          dt = (ft) => {
            const {
              transform: ht,
              d3Selection: bt,
              d3Zoom: Nt,
              translateExtent: ne,
              width: Vt,
              height: Lt,
            } = j.getState();
            if (ft.sourceEvent.type !== "mousemove" || !bt || !Nt) return;
            const se = M.current * Math.max(1, ht[2]) * (N ? -1 : 1),
              Dt = {
                x: ht[0] - ft.sourceEvent.movementX * se,
                y: ht[1] - ft.sourceEvent.movementY * se,
              },
              gt = [
                [0, 0],
                [Vt, Lt],
              ],
              It = Qn.translate(Dt.x, Dt.y).scale(ht[2]),
              Oe = Nt.constrain()(It, gt, ne);
            Nt.transform(bt, Oe);
          },
          tt = Fg()
            .on("zoom", D ? dt : null)
            .on("zoom.wheel", z ? ot : null);
        return (
          it.call(tt),
          () => {
            it.on("zoom", null);
          }
        );
      }
    }, [D, z, N, Y]));
  const $ = _
      ? (it) => {
          const ot = cn(it);
          _(it, { x: ot[0], y: ot[1] });
        }
      : void 0,
    F = E
      ? (it, ot) => {
          const dt = j.getState().nodeInternals.get(ot);
          E(it, dt);
        }
      : void 0;
  return J.createElement(
    pf,
    {
      position: x,
      style: l,
      className: be(["react-flow__minimap", u]),
      "data-testid": "rf__minimap",
    },
    J.createElement(
      "svg",
      {
        width: et,
        height: ct,
        viewBox: `${R} ${Z} ${O} ${k}`,
        role: "img",
        "aria-labelledby": b,
        ref: W,
        onClick: $,
      },
      B && J.createElement("title", { id: b }, B),
      J.createElement(KE, {
        onClick: F,
        nodeColor: s,
        nodeStrokeColor: i,
        nodeBorderRadius: f,
        nodeClassName: o,
        nodeStrokeWidth: d,
        nodeComponent: g,
      }),
      J.createElement("path", {
        className: "react-flow__minimap-mask",
        d: `M${R - S},${Z - S}h${O + S * 2}v${k + S * 2}h${-O - S * 2}z
        M${Q.x},${Q.y}h${Q.width}v${Q.height}h${-Q.width}z`,
        fill: y,
        fillRule: "evenodd",
        stroke: m,
        strokeWidth: p,
        pointerEvents: "none",
      }),
    ),
  );
}
ky.displayName = "MiniMap";
var IE = H.memo(ky);
function PE() {
  return J.createElement(
    "svg",
    { xmlns: "http://www.w3.org/2000/svg", viewBox: "0 0 32 32" },
    J.createElement("path", {
      d: "M32 18.133H18.133V32h-4.266V18.133H0v-4.266h13.867V0h4.266v13.867H32z",
    }),
  );
}
function t_() {
  return J.createElement(
    "svg",
    { xmlns: "http://www.w3.org/2000/svg", viewBox: "0 0 32 5" },
    J.createElement("path", { d: "M0 0h32v4.2H0z" }),
  );
}
function e_() {
  return J.createElement(
    "svg",
    { xmlns: "http://www.w3.org/2000/svg", viewBox: "0 0 32 30" },
    J.createElement("path", {
      d: "M3.692 4.63c0-.53.4-.938.939-.938h5.215V0H4.708C2.13 0 0 2.054 0 4.63v5.216h3.692V4.631zM27.354 0h-5.2v3.692h5.17c.53 0 .984.4.984.939v5.215H32V4.631A4.624 4.624 0 0027.354 0zm.954 24.83c0 .532-.4.94-.939.94h-5.215v3.768h5.215c2.577 0 4.631-2.13 4.631-4.707v-5.139h-3.692v5.139zm-23.677.94c-.531 0-.939-.4-.939-.94v-5.138H0v5.139c0 2.577 2.13 4.707 4.708 4.707h5.138V25.77H4.631z",
    }),
  );
}
function n_() {
  return J.createElement(
    "svg",
    { xmlns: "http://www.w3.org/2000/svg", viewBox: "0 0 25 32" },
    J.createElement("path", {
      d: "M21.333 10.667H19.81V7.619C19.81 3.429 16.38 0 12.19 0 8 0 4.571 3.429 4.571 7.619v3.048H3.048A3.056 3.056 0 000 13.714v15.238A3.056 3.056 0 003.048 32h18.285a3.056 3.056 0 003.048-3.048V13.714a3.056 3.056 0 00-3.048-3.047zM12.19 24.533a3.056 3.056 0 01-3.047-3.047 3.056 3.056 0 013.047-3.048 3.056 3.056 0 013.048 3.048 3.056 3.056 0 01-3.048 3.047zm4.724-13.866H7.467V7.619c0-2.59 2.133-4.724 4.723-4.724 2.591 0 4.724 2.133 4.724 4.724v3.048z",
    }),
  );
}
function l_() {
  return J.createElement(
    "svg",
    { xmlns: "http://www.w3.org/2000/svg", viewBox: "0 0 25 32" },
    J.createElement("path", {
      d: "M21.333 10.667H19.81V7.619C19.81 3.429 16.38 0 12.19 0c-4.114 1.828-1.37 2.133.305 2.438 1.676.305 4.42 2.59 4.42 5.181v3.048H3.047A3.056 3.056 0 000 13.714v15.238A3.056 3.056 0 003.048 32h18.285a3.056 3.056 0 003.048-3.048V13.714a3.056 3.056 0 00-3.048-3.047zM12.19 24.533a3.056 3.056 0 01-3.047-3.047 3.056 3.056 0 013.047-3.048 3.056 3.056 0 013.048 3.048 3.056 3.056 0 01-3.048 3.047z",
    }),
  );
}
const ni = ({ children: l, className: u, ...i }) =>
  J.createElement(
    "button",
    { type: "button", className: be(["react-flow__controls-button", u]), ...i },
    l,
  );
ni.displayName = "ControlButton";
const a_ = (l) => ({
    isInteractive:
      l.nodesDraggable || l.nodesConnectable || l.elementsSelectable,
    minZoomReached: l.transform[2] <= l.minZoom,
    maxZoomReached: l.transform[2] >= l.maxZoom,
  }),
  Ky = ({
    style: l,
    showZoom: u = !0,
    showFitView: i = !0,
    showInteractive: s = !0,
    fitViewOptions: o,
    onZoomIn: f,
    onZoomOut: d,
    onFitView: g,
    onInteractiveChange: y,
    className: m,
    children: p,
    position: x = "bottom-left",
  }) => {
    const _ = fe(),
      [E, D] = H.useState(!1),
      { isInteractive: z, minZoomReached: B, maxZoomReached: N } = Gt(a_, me),
      { zoomIn: Y, zoomOut: G, fitView: j } = Af();
    if (
      (H.useEffect(() => {
        D(!0);
      }, []),
      !E)
    )
      return null;
    const W = () => {
        (Y(), f == null || f());
      },
      I = () => {
        (G(), d == null || d());
      },
      Q = () => {
        (j(o), g == null || g());
      },
      ut = () => {
        (_.setState({
          nodesDraggable: !z,
          nodesConnectable: !z,
          elementsSelectable: !z,
        }),
          y == null || y(!z));
      };
    return J.createElement(
      pf,
      {
        className: be(["react-flow__controls", m]),
        position: x,
        style: l,
        "data-testid": "rf__controls",
      },
      u &&
        J.createElement(
          J.Fragment,
          null,
          J.createElement(
            ni,
            {
              onClick: W,
              className: "react-flow__controls-zoomin",
              title: "zoom in",
              "aria-label": "zoom in",
              disabled: N,
            },
            J.createElement(PE, null),
          ),
          J.createElement(
            ni,
            {
              onClick: I,
              className: "react-flow__controls-zoomout",
              title: "zoom out",
              "aria-label": "zoom out",
              disabled: B,
            },
            J.createElement(t_, null),
          ),
        ),
      i &&
        J.createElement(
          ni,
          {
            className: "react-flow__controls-fitview",
            onClick: Q,
            title: "fit view",
            "aria-label": "fit view",
          },
          J.createElement(e_, null),
        ),
      s &&
        J.createElement(
          ni,
          {
            className: "react-flow__controls-interactive",
            onClick: ut,
            title: "toggle interactivity",
            "aria-label": "toggle interactivity",
          },
          z ? J.createElement(l_, null) : J.createElement(n_, null),
        ),
      p,
    );
  };
Ky.displayName = "Controls";
var u_ = H.memo(Ky),
  rn;
(function (l) {
  ((l.Lines = "lines"), (l.Dots = "dots"), (l.Cross = "cross"));
})(rn || (rn = {}));
function i_({ color: l, dimensions: u, lineWidth: i }) {
  return J.createElement("path", {
    stroke: l,
    strokeWidth: i,
    d: `M${u[0] / 2} 0 V${u[1]} M0 ${u[1] / 2} H${u[0]}`,
  });
}
function c_({ color: l, radius: u }) {
  return J.createElement("circle", { cx: u, cy: u, r: u, fill: l });
}
const s_ = { [rn.Dots]: "#91919a", [rn.Lines]: "#eee", [rn.Cross]: "#e2e2e2" },
  r_ = { [rn.Dots]: 1, [rn.Lines]: 1, [rn.Cross]: 6 },
  o_ = (l) => ({ transform: l.transform, patternId: `pattern-${l.rfId}` });
function $y({
  id: l,
  variant: u = rn.Dots,
  gap: i = 20,
  size: s,
  lineWidth: o = 1,
  offset: f = 2,
  color: d,
  style: g,
  className: y,
}) {
  const m = H.useRef(null),
    { transform: p, patternId: x } = Gt(o_, me),
    _ = d || s_[u],
    E = s || r_[u],
    D = u === rn.Dots,
    z = u === rn.Cross,
    B = Array.isArray(i) ? i : [i, i],
    N = [B[0] * p[2] || 1, B[1] * p[2] || 1],
    Y = E * p[2],
    G = z ? [Y, Y] : N,
    j = D ? [Y / f, Y / f] : [G[0] / f, G[1] / f];
  return J.createElement(
    "svg",
    {
      className: be(["react-flow__background", y]),
      style: {
        ...g,
        position: "absolute",
        width: "100%",
        height: "100%",
        top: 0,
        left: 0,
      },
      ref: m,
      "data-testid": "rf__background",
    },
    J.createElement(
      "pattern",
      {
        id: x + l,
        x: p[0] % N[0],
        y: p[1] % N[1],
        width: N[0],
        height: N[1],
        patternUnits: "userSpaceOnUse",
        patternTransform: `translate(-${j[0]},-${j[1]})`,
      },
      D
        ? J.createElement(c_, { color: _, radius: Y / f })
        : J.createElement(i_, { dimensions: G, color: _, lineWidth: o }),
    ),
    J.createElement("rect", {
      x: "0",
      y: "0",
      width: "100%",
      height: "100%",
      fill: `url(#${x + l})`,
    }),
  );
}
$y.displayName = "Background";
var f_ = H.memo($y);
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const Jy = (...l) =>
  l
    .filter((u, i, s) => !!u && u.trim() !== "" && s.indexOf(u) === i)
    .join(" ")
    .trim();
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const d_ = (l) => l.replace(/([a-z0-9])([A-Z])/g, "$1-$2").toLowerCase();
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const h_ = (l) =>
  l.replace(/^([A-Z])|[\s-_]+(\w)/g, (u, i, s) =>
    s ? s.toUpperCase() : i.toLowerCase(),
  );
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const pg = (l) => {
  const u = h_(l);
  return u.charAt(0).toUpperCase() + u.slice(1);
};
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ var m_ = {
  xmlns: "http://www.w3.org/2000/svg",
  width: 24,
  height: 24,
  viewBox: "0 0 24 24",
  fill: "none",
  stroke: "currentColor",
  strokeWidth: 2,
  strokeLinecap: "round",
  strokeLinejoin: "round",
};
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const g_ = (l) => {
  for (const u in l)
    if (u.startsWith("aria-") || u === "role" || u === "title") return !0;
  return !1;
};
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const y_ = H.forwardRef(
  (
    {
      color: l = "currentColor",
      size: u = 24,
      strokeWidth: i = 2,
      absoluteStrokeWidth: s,
      className: o = "",
      children: f,
      iconNode: d,
      ...g
    },
    y,
  ) =>
    H.createElement(
      "svg",
      {
        ref: y,
        ...m_,
        width: u,
        height: u,
        stroke: l,
        strokeWidth: s ? (Number(i) * 24) / Number(u) : i,
        className: Jy("lucide", o),
        ...(!f && !g_(g) && { "aria-hidden": "true" }),
        ...g,
      },
      [
        ...d.map(([m, p]) => H.createElement(m, p)),
        ...(Array.isArray(f) ? f : [f]),
      ],
    ),
);
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const hs = (l, u) => {
  const i = H.forwardRef(({ className: s, ...o }, f) =>
    H.createElement(y_, {
      ref: f,
      iconNode: u,
      className: Jy(`lucide-${d_(pg(l))}`, `lucide-${l}`, s),
      ...o,
    }),
  );
  return ((i.displayName = pg(l)), i);
};
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const p_ = [
    [
      "path",
      {
        d: "M11 21.73a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73z",
        key: "1a0edw",
      },
    ],
    ["path", { d: "M12 22V12", key: "d0xqtd" }],
    ["polyline", { points: "3.29 7 12 12 20.71 7", key: "ousv84" }],
    ["path", { d: "m7.5 4.27 9 5.15", key: "1c824w" }],
  ],
  v_ = hs("package", p_);
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const x_ = [
    ["path", { d: "m21 21-4.34-4.34", key: "14j7rj" }],
    ["circle", { cx: "11", cy: "11", r: "8", key: "4ej97u" }],
  ],
  b_ = hs("search", x_);
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const S_ = [
    [
      "path",
      {
        d: "M9.671 4.136a2.34 2.34 0 0 1 4.659 0 2.34 2.34 0 0 0 3.319 1.915 2.34 2.34 0 0 1 2.33 4.033 2.34 2.34 0 0 0 0 3.831 2.34 2.34 0 0 1-2.33 4.033 2.34 2.34 0 0 0-3.319 1.915 2.34 2.34 0 0 1-4.659 0 2.34 2.34 0 0 0-3.32-1.915 2.34 2.34 0 0 1-2.33-4.033 2.34 2.34 0 0 0 0-3.831A2.34 2.34 0 0 1 6.35 6.051a2.34 2.34 0 0 0 3.319-1.915",
        key: "1i5ecw",
      },
    ],
    ["circle", { cx: "12", cy: "12", r: "3", key: "1v7zrd" }],
  ],
  E_ = hs("settings", S_);
/**
 * @license lucide-react v0.563.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */ const __ = [
    ["path", { d: "M18 6 6 18", key: "1bl5f8" }],
    ["path", { d: "m6 6 12 12", key: "d8bk6v" }],
  ],
  w_ = hs("x", __),
  Wy = {},
  { useDebugValue: N_ } = J,
  { useSyncExternalStoreWithSelector: A_ } = Sg;
let vg = !1;
const T_ = (l) => l;
function z_(l, u = T_, i) {
  (Wy ? "production" : void 0) !== "production" &&
    i &&
    !vg &&
    (console.warn(
      "[DEPRECATED] Use `createWithEqualityFn` instead of `create` or use `useStoreWithEqualityFn` instead of `useStore`. They can be imported from 'zustand/traditional'. https://github.com/pmndrs/zustand/discussions/1937",
    ),
    (vg = !0));
  const s = A_(
    l.subscribe,
    l.getState,
    l.getServerState || l.getInitialState,
    u,
    i,
  );
  return (N_(s), s);
}
const xg = (l) => {
    (Wy ? "production" : void 0) !== "production" &&
      typeof l != "function" &&
      console.warn(
        "[DEPRECATED] Passing a vanilla store will be unsupported in a future version. Instead use `import { useStore } from 'zustand'`.",
      );
    const u = typeof l == "function" ? Eg(l) : l,
      i = (s, o) => z_(u, s, o);
    return (Object.assign(i, u), i);
  },
  M_ = (l) => (l ? xg(l) : xg),
  Ia = M_((l, u) => ({
    workflow: {
      id: "new-workflow",
      name: "新しいワークフロー",
      description: "",
      nodes: [],
      edges: [],
    },
    selectedNode: null,
    history: [],
    historyIndex: -1,
    updateWorkflow: (i) => {
      l((s) => {
        const o = { ...s.workflow, ...i },
          f = s.history.slice(0, s.historyIndex + 1);
        return (
          f.push(o),
          { workflow: o, history: f, historyIndex: f.length - 1 }
        );
      });
    },
    setSelectedNode: (i) => {
      l({ selectedNode: i });
    },
    updateNodeData: (i, s) => {
      l((o) => {
        const f = o.workflow.nodes.map((d) =>
          d.id === i ? { ...d, data: s } : d,
        );
        return { workflow: { ...o.workflow, nodes: f } };
      });
    },
    saveWorkflow: async () => {
      const { workflow: i } = u();
      try {
        if (
          !(
            await fetch(`/api/workflows/${i.id}`, {
              method: "PUT",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify({
                name: i.name,
                description: i.description,
                nodes: i.nodes,
                edges: i.edges,
              }),
            })
          ).ok
        )
          throw new Error("Failed to save workflow");
        console.log("Workflow saved successfully");
      } catch (s) {
        throw (console.error("Failed to save workflow:", s), s);
      }
    },
    loadWorkflow: async (i) => {
      try {
        const s = await fetch(`/api/workflows/${i}`);
        if (!s.ok) throw new Error("Failed to load workflow");
        const o = await s.json();
        (l({
          workflow: {
            id: o.id,
            name: o.name,
            description: o.description,
            nodes: o.nodes || [],
            edges: o.edges || [],
          },
          history: [],
          historyIndex: -1,
        }),
          console.log("Workflow loaded successfully"));
      } catch (s) {
        throw (console.error("Failed to load workflow:", s), s);
      }
    },
    undo: () => {
      l((i) => {
        if (i.historyIndex > 0) {
          const s = i.historyIndex - 1;
          return { workflow: i.history[s], historyIndex: s };
        }
        return i;
      });
    },
    redo: () => {
      l((i) => {
        if (i.historyIndex < i.history.length - 1) {
          const s = i.historyIndex + 1;
          return { workflow: i.history[s], historyIndex: s };
        }
        return i;
      });
    },
    canUndo: () => {
      const { historyIndex: i } = u();
      return i > 0;
    },
    canRedo: () => {
      const { historyIndex: i, history: s } = u();
      return i < s.length - 1;
    },
  }));
function C_({ id: l, data: u, selected: i }) {
  const { setSelectedNode: s } = Ia(),
    o = () => {
      s(l);
    };
  return A.jsxs("div", {
    className: `
        px-4 py-3 rounded-lg border-2 bg-card shadow-lg
        min-w-[200px] transition-all
        ${i ? "border-primary ring-2 ring-primary/20" : "border-border"}
      `,
    children: [
      A.jsx(Fa, {
        type: "target",
        position: pt.Top,
        className: "w-3 h-3 !bg-primary",
      }),
      A.jsxs("div", {
        className: "flex items-center justify-between gap-2 mb-2",
        children: [
          A.jsxs("div", {
            className: "flex-1",
            children: [
              A.jsx("div", {
                className: "text-sm font-semibold text-foreground",
                children: u.label,
              }),
              A.jsx("div", {
                className: "text-xs text-muted-foreground",
                children: u.agentId,
              }),
            ],
          }),
          A.jsx("button", {
            onClick: o,
            className: "p-1 rounded hover:bg-accent transition-colors",
            title: "設定を開く",
            children: A.jsx(E_, { className: "w-4 h-4 text-muted-foreground" }),
          }),
        ],
      }),
      Object.keys(u.config).length > 0 &&
        A.jsxs("div", {
          className:
            "text-xs text-muted-foreground border-t border-border pt-2 mt-2",
          children: [Object.keys(u.config).length, " 個の設定"],
        }),
      A.jsx(Fa, {
        type: "source",
        position: pt.Bottom,
        className: "w-3 h-3 !bg-primary",
      }),
    ],
  });
}
const D_ = H.memo(C_),
  O_ = { agent: D_ };
function R_() {
  const { workflow: l, updateWorkflow: u } = Ia(),
    [i, s, o] = VE(l.nodes),
    [f, d, g] = LE(l.edges),
    y = H.useCallback(
      (E) => {
        const D = ry(E, f);
        (d(D), u({ edges: D }));
      },
      [f, d, u],
    ),
    m = H.useCallback((E) => {
      (E.preventDefault(), (E.dataTransfer.dropEffect = "move"));
    }, []),
    p = H.useCallback(
      (E) => {
        E.preventDefault();
        const D = E.dataTransfer.getData("application/reactflow");
        if (!D) return;
        const z = JSON.parse(D),
          B = { x: E.clientX - 250, y: E.clientY - 40 },
          N = {
            id: `${z.id}-${Date.now()}`,
            type: "agent",
            position: B,
            data: { label: z.name, agentId: z.id, config: {} },
          },
          Y = [...i, N];
        (s(Y), u({ nodes: Y }));
      },
      [i, s, u],
    ),
    x = H.useCallback(
      (E) => {
        (o(E),
          setTimeout(() => {
            u({ nodes: i });
          }, 0));
      },
      [i, o, u],
    ),
    _ = H.useCallback(
      (E) => {
        (g(E),
          setTimeout(() => {
            u({ edges: f });
          }, 0));
      },
      [f, g, u],
    );
  return A.jsx("div", {
    className: "w-full h-full",
    children: A.jsxs(Gy, {
      nodes: i,
      edges: f,
      onNodesChange: x,
      onEdgesChange: _,
      onConnect: y,
      onDragOver: m,
      onDrop: p,
      nodeTypes: O_,
      fitView: !0,
      className: "bg-background",
      children: [
        A.jsx(f_, {}),
        A.jsx(u_, {}),
        A.jsx(IE, {
          nodeColor: (E) => {
            switch (E.type) {
              case "agent":
                return "#3b82f6";
              default:
                return "#6b7280";
            }
          },
          className: "bg-card border border-border",
        }),
      ],
    }),
  });
}
function H_() {
  const [l, u] = H.useState([]),
    [i, s] = H.useState(""),
    [o, f] = H.useState(!0);
  H.useEffect(() => {
    d();
  }, []);
  const d = async () => {
      try {
        const p = await (await fetch("/api/agents")).json();
        u(p);
      } catch (m) {
        console.error("Failed to fetch agents:", m);
      } finally {
        f(!1);
      }
    },
    g = (m, p) => {
      (m.dataTransfer.setData("application/reactflow", JSON.stringify(p)),
        (m.dataTransfer.effectAllowed = "move"));
    },
    y = l.filter(
      (m) =>
        m.name.toLowerCase().includes(i.toLowerCase()) ||
        m.description.toLowerCase().includes(i.toLowerCase()),
    );
  return A.jsxs("div", {
    className: "w-64 border-r border-border bg-card flex flex-col",
    children: [
      A.jsxs("div", {
        className: "p-4 border-b border-border",
        children: [
          A.jsx("h2", {
            className: "text-lg font-semibold text-foreground mb-3",
            children: "エージェント",
          }),
          A.jsxs("div", {
            className: "relative",
            children: [
              A.jsx(b_, {
                className:
                  "absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-muted-foreground",
              }),
              A.jsx("input", {
                type: "text",
                placeholder: "検索...",
                value: i,
                onChange: (m) => s(m.target.value),
                className:
                  "w-full pl-9 pr-3 py-2 text-sm rounded-md border border-input bg-background text-foreground placeholder:text-muted-foreground focus:outline-none focus:ring-2 focus:ring-ring",
              }),
            ],
          }),
        ],
      }),
      A.jsx("div", {
        className: "flex-1 overflow-y-auto p-2",
        children: o
          ? A.jsx("div", {
              className:
                "flex items-center justify-center h-32 text-muted-foreground",
              children: "読み込み中...",
            })
          : y.length === 0
            ? A.jsxs("div", {
                className:
                  "flex flex-col items-center justify-center h-32 text-muted-foreground text-sm",
                children: [
                  A.jsx(v_, { className: "w-8 h-8 mb-2" }),
                  A.jsx("p", { children: "エージェントが見つかりません" }),
                ],
              })
            : A.jsx("div", {
                className: "space-y-2",
                children: y.map((m) =>
                  A.jsxs(
                    "div",
                    {
                      draggable: !0,
                      onDragStart: (p) => g(p, m),
                      className:
                        "p-3 rounded-lg border border-border bg-background hover:bg-accent cursor-move transition-colors",
                      children: [
                        A.jsxs("div", {
                          className:
                            "flex items-start justify-between gap-2 mb-1",
                          children: [
                            A.jsx("div", {
                              className: "font-medium text-sm text-foreground",
                              children: m.name,
                            }),
                            A.jsxs("div", {
                              className: "text-xs text-muted-foreground",
                              children: ["v", m.version],
                            }),
                          ],
                        }),
                        A.jsx("div", {
                          className:
                            "text-xs text-muted-foreground line-clamp-2",
                          children: m.description,
                        }),
                        A.jsx("div", {
                          className: "mt-2",
                          children: A.jsx("span", {
                            className:
                              "inline-block px-2 py-0.5 text-xs rounded-full bg-primary/10 text-primary",
                            children: m.category,
                          }),
                        }),
                      ],
                    },
                    m.id,
                  ),
                ),
              }),
      }),
      A.jsxs("div", {
        className: "p-4 border-t border-border text-xs text-muted-foreground",
        children: [y.length, " 個のエージェント"],
      }),
    ],
  });
}
function j_() {
  const {
      selectedNode: l,
      workflow: u,
      setSelectedNode: i,
      updateNodeData: s,
    } = Ia(),
    o = u.nodes.find((g) => g.id === l);
  if (!o) return null;
  const f = () => {
      i(null);
    },
    d = (g, y) => {
      s(o.id, { ...o.data, config: { ...o.data.config, [g]: y } });
    };
  return A.jsxs("div", {
    className: "w-80 border-l border-border bg-card flex flex-col",
    children: [
      A.jsxs("div", {
        className:
          "p-4 border-b border-border flex items-center justify-between",
        children: [
          A.jsx("h2", {
            className: "text-lg font-semibold text-foreground",
            children: "プロパティ",
          }),
          A.jsx("button", {
            onClick: f,
            className: "p-1 rounded hover:bg-accent transition-colors",
            title: "閉じる",
            children: A.jsx(w_, { className: "w-4 h-4 text-muted-foreground" }),
          }),
        ],
      }),
      A.jsxs("div", {
        className: "flex-1 overflow-y-auto p-4 space-y-4",
        children: [
          A.jsxs("div", {
            children: [
              A.jsx("label", {
                className: "block text-sm font-medium text-foreground mb-2",
                children: "ノード名",
              }),
              A.jsx("input", {
                type: "text",
                value: o.data.label,
                onChange: (g) => s(o.id, { ...o.data, label: g.target.value }),
                className:
                  "w-full px-3 py-2 text-sm rounded-md border border-input bg-background text-foreground focus:outline-none focus:ring-2 focus:ring-ring",
              }),
            ],
          }),
          A.jsxs("div", {
            children: [
              A.jsx("label", {
                className: "block text-sm font-medium text-foreground mb-2",
                children: "エージェント ID",
              }),
              A.jsx("div", {
                className:
                  "px-3 py-2 text-sm rounded-md border border-input bg-muted text-muted-foreground",
                children: o.data.agentId,
              }),
            ],
          }),
          A.jsxs("div", {
            children: [
              A.jsx("label", {
                className: "block text-sm font-medium text-foreground mb-2",
                children: "設定",
              }),
              A.jsxs("div", {
                className: "space-y-3",
                children: [
                  A.jsxs("div", {
                    children: [
                      A.jsx("label", {
                        className: "block text-xs text-muted-foreground mb-1",
                        children: "入力データ",
                      }),
                      A.jsx("textarea", {
                        value: o.data.config.input || "",
                        onChange: (g) => d("input", g.target.value),
                        placeholder: '{"key": "value"}',
                        className:
                          "w-full px-3 py-2 text-sm rounded-md border border-input bg-background text-foreground font-mono focus:outline-none focus:ring-2 focus:ring-ring",
                        rows: 4,
                      }),
                    ],
                  }),
                  A.jsxs("div", {
                    children: [
                      A.jsx("label", {
                        className: "block text-xs text-muted-foreground mb-1",
                        children: "タイムアウト (秒)",
                      }),
                      A.jsx("input", {
                        type: "number",
                        value: o.data.config.timeout || 30,
                        onChange: (g) => d("timeout", parseInt(g.target.value)),
                        className:
                          "w-full px-3 py-2 text-sm rounded-md border border-input bg-background text-foreground focus:outline-none focus:ring-2 focus:ring-ring",
                      }),
                    ],
                  }),
                  A.jsx("div", {
                    children: A.jsxs("label", {
                      className:
                        "flex items-center gap-2 text-sm text-foreground cursor-pointer",
                      children: [
                        A.jsx("input", {
                          type: "checkbox",
                          checked: o.data.config.enabled !== !1,
                          onChange: (g) => d("enabled", g.target.checked),
                          className:
                            "w-4 h-4 rounded border-input text-primary focus:ring-2 focus:ring-ring",
                        }),
                        "有効化",
                      ],
                    }),
                  }),
                ],
              }),
            ],
          }),
          A.jsxs("div", {
            children: [
              A.jsx("label", {
                className: "block text-sm font-medium text-foreground mb-2",
                children: "位置",
              }),
              A.jsxs("div", {
                className: "grid grid-cols-2 gap-2",
                children: [
                  A.jsxs("div", {
                    children: [
                      A.jsx("label", {
                        className: "block text-xs text-muted-foreground mb-1",
                        children: "X",
                      }),
                      A.jsx("div", {
                        className:
                          "px-3 py-2 text-sm rounded-md border border-input bg-muted text-muted-foreground",
                        children: Math.round(o.position.x),
                      }),
                    ],
                  }),
                  A.jsxs("div", {
                    children: [
                      A.jsx("label", {
                        className: "block text-xs text-muted-foreground mb-1",
                        children: "Y",
                      }),
                      A.jsx("div", {
                        className:
                          "px-3 py-2 text-sm rounded-md border border-input bg-muted text-muted-foreground",
                        children: Math.round(o.position.y),
                      }),
                    ],
                  }),
                ],
              }),
            ],
          }),
        ],
      }),
      A.jsx("div", {
        className: "p-4 border-t border-border",
        children: A.jsx("button", {
          onClick: f,
          className:
            "w-full px-4 py-2 text-sm font-medium rounded-md bg-primary text-primary-foreground hover:bg-primary/90 transition-colors",
          children: "完了",
        }),
      }),
    ],
  });
}
function U_() {
  const { workflow: l } = Ia(),
    [u, i] = H.useState(`{

}`),
    [s, o] = H.useState(null),
    [f, d] = H.useState(null),
    [g, y] = H.useState(!1),
    [m, p] = H.useState("input"),
    [x, _] = H.useState(!1),
    E = H.useCallback((N) => {
      try {
        return (JSON.parse(N), o(null), !0);
      } catch (Y) {
        return (o(Y.message), !1);
      }
    }, []),
    D = H.useCallback(
      (N) => {
        (i(N), E(N));
      },
      [E],
    ),
    z = H.useCallback(async () => {
      if (E(u)) {
        (y(!0), d(null), p("output"));
        try {
          const N = await fetch("/api/preview/run", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              workflow: {
                id: l.id,
                name: l.name,
                description: l.description,
                nodes: l.nodes,
                edges: l.edges,
              },
              input_data: JSON.parse(u),
              debug: x,
            }),
          });
          if (!N.ok) throw new Error(`HTTP error! status: ${N.status}`);
          const Y = await N.json();
          (d(Y), Y.logs && Y.logs.length > 0 && p("logs"));
        } catch (N) {
          d({
            status: "error",
            result: null,
            logs: [],
            duration_ms: null,
            error: N.message,
          });
        } finally {
          y(!1);
        }
      }
    }, [l, u, x, E]),
    B = H.useCallback(async () => {
      var N;
      if (E(u)) {
        (y(!0),
          d({
            status: "running",
            result: null,
            logs: [],
            duration_ms: null,
            error: null,
          }),
          p("logs"));
        try {
          const G =
            (N = (
              await fetch("/api/preview/stream", {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({
                  workflow: {
                    id: l.id,
                    name: l.name,
                    description: l.description,
                    nodes: l.nodes,
                    edges: l.edges,
                  },
                  input_data: JSON.parse(u),
                  debug: x,
                }),
              })
            ).body) == null
              ? void 0
              : N.getReader();
          if (!G) throw new Error("Failed to get response reader");
          const j = new TextDecoder(),
            W = [];
          for (;;) {
            const { done: I, value: Q } = await G.read();
            if (I) break;
            const et = j.decode(Q).split(`
`);
            for (const ct of et)
              if (ct.startsWith("data: "))
                try {
                  const at = JSON.parse(ct.slice(6));
                  (W.push({ ...at, timestamp: Date.now() }),
                    d((st) => ({
                      ...st,
                      logs: [...W],
                      result:
                        at.type === "complete"
                          ? at.result
                          : st == null
                            ? void 0
                            : st.result,
                      status: at.type === "complete" ? "success" : "running",
                    })));
                } catch {}
          }
        } catch (Y) {
          d((G) => ({ ...G, status: "error", error: Y.message }));
        } finally {
          y(!1);
        }
      }
    }, [l, u, x, E]);
  return A.jsxs("div", {
    className: "w-80 border-l bg-background flex flex-col",
    children: [
      A.jsxs("div", {
        className: "p-4 border-b",
        children: [
          A.jsxs("div", {
            className: "flex items-center justify-between mb-3",
            children: [
              A.jsx("h3", {
                className: "font-semibold text-sm",
                children: "Preview",
              }),
              A.jsxs("div", {
                className: "flex gap-1",
                children: [
                  A.jsx("button", {
                    onClick: z,
                    disabled: g || l.nodes.length === 0,
                    className:
                      "px-3 py-1.5 bg-primary text-primary-foreground text-xs rounded-md hover:bg-primary/90 disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-1",
                    children: g
                      ? A.jsxs(A.Fragment, {
                          children: [
                            A.jsx("span", {
                              className: "animate-spin",
                              children: "⏳",
                            }),
                            "Running...",
                          ],
                        })
                      : A.jsxs(A.Fragment, {
                          children: [A.jsx("span", { children: "▶" }), "Run"],
                        }),
                  }),
                  A.jsx("button", {
                    onClick: B,
                    disabled: g || l.nodes.length === 0,
                    className:
                      "px-2 py-1.5 bg-secondary text-secondary-foreground text-xs rounded-md hover:bg-secondary/90 disabled:opacity-50 disabled:cursor-not-allowed",
                    title: "Stream 実行",
                    children: "📡",
                  }),
                ],
              }),
            ],
          }),
          A.jsxs("label", {
            className: "flex items-center gap-2 text-xs text-muted-foreground",
            children: [
              A.jsx("input", {
                type: "checkbox",
                checked: x,
                onChange: (N) => _(N.target.checked),
                className: "rounded",
              }),
              "Debug モード（中間結果を表示）",
            ],
          }),
        ],
      }),
      A.jsx("div", {
        className: "flex border-b",
        children: ["input", "output", "logs"].map((N) => {
          var Y;
          return A.jsxs(
            "button",
            {
              onClick: () => p(N),
              className: `flex-1 px-3 py-2 text-xs font-medium border-b-2 transition-colors ${m === N ? "border-primary text-primary" : "border-transparent text-muted-foreground hover:text-foreground"}`,
              children: [
                N === "input" && "📝 Input",
                N === "output" && "📊 Output",
                N === "logs" &&
                  `📋 Logs ${(Y = f == null ? void 0 : f.logs) != null && Y.length ? `(${f.logs.length})` : ""}`,
              ],
            },
            N,
          );
        }),
      }),
      A.jsxs("div", {
        className: "flex-1 overflow-auto p-3",
        children: [
          m === "input" &&
            A.jsxs("div", {
              className: "space-y-2",
              children: [
                A.jsx("label", {
                  className: "text-xs text-muted-foreground",
                  children: "入力データ (JSON)",
                }),
                A.jsx("textarea", {
                  value: u,
                  onChange: (N) => D(N.target.value),
                  className: `w-full h-48 p-2 text-xs font-mono bg-muted rounded-md resize-none focus:outline-none focus:ring-2 ${s ? "ring-2 ring-destructive" : "focus:ring-primary"}`,
                  placeholder: '{\\n  "key": "value"\\n}',
                }),
                s &&
                  A.jsx("p", {
                    className: "text-xs text-destructive",
                    children: s,
                  }),
              ],
            }),
          m === "output" &&
            A.jsx("div", {
              className: "space-y-3",
              children: f
                ? A.jsxs(A.Fragment, {
                    children: [
                      A.jsxs("div", {
                        className: "flex items-center gap-2 text-xs",
                        children: [
                          A.jsx("span", {
                            className: `px-2 py-0.5 rounded-full ${f.status === "success" ? "bg-green-100 text-green-700" : f.status === "error" ? "bg-red-100 text-red-700" : "bg-yellow-100 text-yellow-700"}`,
                            children: f.status,
                          }),
                          f.duration_ms &&
                            A.jsxs("span", {
                              className: "text-muted-foreground",
                              children: [f.duration_ms.toFixed(1), "ms"],
                            }),
                        ],
                      }),
                      f.result &&
                        A.jsxs("div", {
                          children: [
                            A.jsx("label", {
                              className:
                                "text-xs text-muted-foreground mb-1 block",
                              children: "実行結果",
                            }),
                            A.jsx("pre", {
                              className:
                                "p-2 bg-muted rounded-md text-xs font-mono overflow-auto max-h-64",
                              children: JSON.stringify(f.result, null, 2),
                            }),
                          ],
                        }),
                      f.error &&
                        A.jsx("div", {
                          className:
                            "p-2 bg-destructive/10 border border-destructive/20 rounded-md",
                          children: A.jsx("p", {
                            className: "text-xs text-destructive",
                            children: f.error,
                          }),
                        }),
                    ],
                  })
                : A.jsx("p", {
                    className: "text-xs text-muted-foreground text-center py-8",
                    children: "Run をクリックして実行結果を確認",
                  }),
            }),
          m === "logs" &&
            A.jsx("div", {
              className: "space-y-2",
              children:
                f != null && f.logs && f.logs.length > 0
                  ? f.logs.map((N, Y) =>
                      A.jsxs(
                        "div",
                        {
                          className: `p-2 rounded-md text-xs ${N.type === "error" ? "bg-destructive/10 text-destructive" : N.type === "complete" ? "bg-green-50 text-green-700" : N.type === "progress" ? "bg-blue-50 text-blue-700" : "bg-muted"}`,
                          children: [
                            A.jsxs("div", {
                              className: "flex items-center gap-2",
                              children: [
                                A.jsxs("span", {
                                  children: [
                                    N.type === "error" && "❌",
                                    N.type === "complete" && "✅",
                                    N.type === "progress" && "⏳",
                                    N.type === "info" && "ℹ️",
                                  ],
                                }),
                                N.node_id &&
                                  A.jsx("span", {
                                    className:
                                      "font-mono text-xs bg-background px-1 rounded",
                                    children: N.node_id,
                                  }),
                                N.agent_type &&
                                  A.jsx("span", {
                                    className: "text-muted-foreground",
                                    children: N.agent_type,
                                  }),
                              ],
                            }),
                            N.message &&
                              A.jsx("p", {
                                className: "mt-1",
                                children: N.message,
                              }),
                            N.status &&
                              A.jsxs("p", {
                                className: "mt-1 text-muted-foreground",
                                children: ["Status: ", N.status],
                              }),
                          ],
                        },
                        Y,
                      ),
                    )
                  : A.jsx("p", {
                      className:
                        "text-xs text-muted-foreground text-center py-8",
                      children: "実行ログがありません",
                    }),
            }),
        ],
      }),
      A.jsx("div", {
        className: "p-3 border-t bg-muted/30",
        children: A.jsxs("p", {
          className: "text-xs text-muted-foreground",
          children: [l.nodes.length, " ノード · ", l.edges.length, " エッジ"],
        }),
      }),
    ],
  });
}
function B_({ open: l, onClose: u }) {
  const { workflow: i } = Ia(),
    [s, o] = H.useState([]),
    [f, d] = H.useState("backend"),
    [g, y] = H.useState([]),
    [m, p] = H.useState("vercel"),
    [x, _] = H.useState([]),
    [E, D] = H.useState({}),
    [z, B] = H.useState(""),
    [N, Y] = H.useState("1.0.0"),
    [G, j] = H.useState(!0),
    [W, I] = H.useState(!0),
    [Q, ut] = H.useState(null),
    [et, ct] = H.useState(!1),
    [at, st] = H.useState(null),
    [rt, w] = H.useState(!1),
    [L, S] = H.useState(!1),
    [R, Z] = H.useState([]),
    [O, k] = H.useState(null),
    [b, M] = H.useState("export");
  (H.useEffect(() => {
    l &&
      fetch("/api/publish/targets")
        .then((tt) => tt.json())
        .then((tt) => {
          (o(tt.output_types || []), y(tt.deploy_targets || []));
        })
        .catch(console.error);
  }, [l]),
    H.useEffect(() => {
      i.name && !z && B(i.name.toLowerCase().replace(/\s+/g, "-"));
    }, [i.name, z]),
    H.useEffect(() => {
      m &&
        b === "deploy" &&
        fetch(`/api/publish/config-fields/${m}`)
          .then((tt) => tt.json())
          .then((tt) => {
            _(tt);
            const ft = {};
            (tt.forEach((ht) => {
              ht.default !== void 0 && (ft[ht.name] = ht.default);
            }),
              D((ht) => ({ ...ft, ...ht })));
          })
          .catch(console.error);
    }, [m, b]));
  const $ = (tt) => {
      var ht;
      const ft = E[tt.name];
      switch (tt.type) {
        case "password":
          return A.jsx("input", {
            type: "password",
            value: ft || "",
            onChange: (bt) =>
              D((Nt) => ({ ...Nt, [tt.name]: bt.target.value })),
            className: "w-full px-3 py-2 border rounded-md text-sm",
            placeholder: tt.placeholder || "••••••••",
          });
        case "select":
          return A.jsx("select", {
            value: String(ft || tt.default || ""),
            onChange: (bt) =>
              D((Nt) => ({ ...Nt, [tt.name]: bt.target.value })),
            className: "w-full px-3 py-2 border rounded-md text-sm",
            children:
              (ht = tt.options) == null
                ? void 0
                : ht.map((bt) =>
                    A.jsx("option", { value: bt, children: bt }, bt),
                  ),
          });
        case "boolean":
          return A.jsxs("label", {
            className: "flex items-center gap-2",
            children: [
              A.jsx("input", {
                type: "checkbox",
                checked: !!ft,
                onChange: (bt) =>
                  D((Nt) => ({ ...Nt, [tt.name]: bt.target.checked })),
                className: "rounded",
              }),
              A.jsx("span", { className: "text-sm", children: tt.description }),
            ],
          });
        case "number":
          return A.jsx("input", {
            type: "number",
            value: Number(ft || tt.default || 0),
            onChange: (bt) =>
              D((Nt) => ({ ...Nt, [tt.name]: parseInt(bt.target.value) })),
            className: "w-full px-3 py-2 border rounded-md text-sm",
            placeholder: tt.placeholder,
          });
        case "textarea":
          return A.jsx("textarea", {
            value: ft || "",
            onChange: (bt) =>
              D((Nt) => ({ ...Nt, [tt.name]: bt.target.value })),
            className: "w-full px-3 py-2 border rounded-md text-sm min-h-20",
            placeholder: tt.placeholder,
          });
        default:
          return A.jsx("input", {
            type: "text",
            value: ft || "",
            onChange: (bt) =>
              D((Nt) => ({ ...Nt, [tt.name]: bt.target.value })),
            className: "w-full px-3 py-2 border rounded-md text-sm",
            placeholder: tt.placeholder,
          });
      }
    },
    F = x.reduce((tt, ft) => {
      const ht = ft.group || "general";
      return (tt[ht] || (tt[ht] = []), tt[ht].push(ft), tt);
    }, {}),
    it = H.useCallback(async () => {
      (ct(!0), ut(null));
      try {
        const ft = await (
          await fetch("/api/publish/preview", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              workflow: {
                id: i.id,
                name: i.name,
                description: i.description,
                nodes: i.nodes,
                edges: i.edges,
              },
              target: f,
              app_name: z,
              version: N,
              include_tests: G,
              include_readme: W,
            }),
          })
        ).json();
        if (ft.status === "success") {
          ut(ft.files);
          const ht = Object.keys(ft.files)[0];
          st(ht || null);
        }
      } catch (tt) {
        console.error("Preview failed:", tt);
      } finally {
        ct(!1);
      }
    }, [i, f, z, N, G, W]),
    ot = H.useCallback(async () => {
      w(!0);
      try {
        const tt = await fetch("/api/publish/export", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            workflow: {
              id: i.id,
              name: i.name,
              description: i.description,
              nodes: i.nodes,
              edges: i.edges,
            },
            target: f,
            app_name: z,
            version: N,
            include_tests: G,
            include_readme: W,
          }),
        });
        if (!tt.ok) throw new Error("Export failed");
        const ft = await tt.blob(),
          ht = URL.createObjectURL(ft),
          bt = document.createElement("a");
        ((bt.href = ht),
          (bt.download = `${z || "workflow"}-${f}.zip`),
          document.body.appendChild(bt),
          bt.click(),
          document.body.removeChild(bt),
          URL.revokeObjectURL(ht));
      } catch (tt) {
        (console.error("Export failed:", tt),
          alert("エクスポートに失敗しました"));
      } finally {
        w(!1);
      }
    }, [i, f, z, N, G, W]),
    dt = H.useCallback(async () => {
      var tt, ft;
      (S(!0), k(null), Z([]));
      try {
        const bt =
          (tt = (
            await fetch("/api/publish/deploy/stream", {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify({
                workflow: {
                  id: i.id,
                  name: i.name,
                  description: i.description,
                  nodes: i.nodes,
                  edges: i.edges,
                },
                target: m,
                app_name: z || E.project_name,
                credentials: E,
              }),
            })
          ).body) == null
            ? void 0
            : tt.getReader();
        if (!bt) return;
        const Nt = new TextDecoder();
        for (;;) {
          const { done: ne, value: Vt } = await bt.read();
          if (ne) break;
          const se = Nt.decode(Vt).split(`
`);
          for (const Dt of se)
            if (Dt.startsWith("data: "))
              try {
                const gt = JSON.parse(Dt.slice(6));
                (Z((It) => [...It, gt.message]),
                  gt.type === "success"
                    ? k({
                        status: "success",
                        url: (ft = gt.data) == null ? void 0 : ft.url,
                      })
                    : gt.type === "error" &&
                      k({ status: "error", error: gt.message }));
              } catch {}
        }
      } catch (ht) {
        k({ status: "error", error: ht.message });
      } finally {
        S(!1);
      }
    }, [i, m, z, E]);
  return l
    ? A.jsxs("div", {
        className: "fixed inset-0 z-50 flex items-center justify-center",
        children: [
          A.jsx("div", {
            className: "absolute inset-0 bg-black/50",
            onClick: u,
          }),
          A.jsxs("div", {
            className:
              "relative bg-background rounded-lg shadow-xl w-[900px] max-h-[85vh] flex flex-col",
            children: [
              A.jsxs("div", {
                className: "p-4 border-b flex items-center justify-between",
                children: [
                  A.jsxs("div", {
                    children: [
                      A.jsx("h2", {
                        className: "text-lg font-semibold",
                        children: "ワークフローを発行",
                      }),
                      A.jsxs("p", {
                        className: "text-sm text-muted-foreground",
                        children: [i.name, " をエクスポートまたはデプロイ"],
                      }),
                    ],
                  }),
                  A.jsx("button", {
                    onClick: u,
                    className: "p-2 hover:bg-muted rounded-md",
                    children: "✕",
                  }),
                ],
              }),
              A.jsxs("div", {
                className: "flex border-b",
                children: [
                  A.jsx("button", {
                    onClick: () => M("export"),
                    className: `px-4 py-2 text-sm font-medium border-b-2 transition-colors ${b === "export" ? "border-primary text-primary" : "border-transparent hover:text-foreground"}`,
                    children: "📦 エクスポート",
                  }),
                  A.jsx("button", {
                    onClick: () => M("deploy"),
                    className: `px-4 py-2 text-sm font-medium border-b-2 transition-colors ${b === "deploy" ? "border-primary text-primary" : "border-transparent hover:text-foreground"}`,
                    children: "🚀 デプロイ",
                  }),
                ],
              }),
              A.jsx("div", {
                className: "flex-1 overflow-auto p-4",
                children: A.jsxs("div", {
                  className: "grid grid-cols-2 gap-6",
                  children: [
                    A.jsx("div", {
                      className: "space-y-4",
                      children:
                        b === "export"
                          ? A.jsxs(A.Fragment, {
                              children: [
                                A.jsxs("div", {
                                  children: [
                                    A.jsx("label", {
                                      className:
                                        "text-sm font-medium mb-2 block",
                                      children: "出力タイプ",
                                    }),
                                    A.jsx("div", {
                                      className: "grid grid-cols-1 gap-2",
                                      children: s.map((tt) =>
                                        A.jsxs(
                                          "button",
                                          {
                                            onClick: () => d(tt.id),
                                            className: `p-3 border rounded-lg text-left transition-colors ${f === tt.id ? "border-primary bg-primary/5" : "hover:border-muted-foreground/50"}`,
                                            children: [
                                              A.jsxs("div", {
                                                className:
                                                  "flex items-center gap-2",
                                                children: [
                                                  A.jsx("span", {
                                                    className: "text-lg",
                                                    children: tt.icon,
                                                  }),
                                                  A.jsx("span", {
                                                    className:
                                                      "font-medium text-sm",
                                                    children: tt.name,
                                                  }),
                                                ],
                                              }),
                                              A.jsx("p", {
                                                className:
                                                  "text-xs text-muted-foreground mt-1",
                                                children: tt.description,
                                              }),
                                            ],
                                          },
                                          tt.id,
                                        ),
                                      ),
                                    }),
                                  ],
                                }),
                                A.jsxs("div", {
                                  children: [
                                    A.jsx("label", {
                                      className:
                                        "text-sm font-medium mb-1 block",
                                      children: "アプリケーション名",
                                    }),
                                    A.jsx("input", {
                                      type: "text",
                                      value: z,
                                      onChange: (tt) => B(tt.target.value),
                                      className:
                                        "w-full px-3 py-2 border rounded-md text-sm",
                                      placeholder: "my-workflow",
                                    }),
                                  ],
                                }),
                                A.jsxs("div", {
                                  children: [
                                    A.jsx("label", {
                                      className:
                                        "text-sm font-medium mb-1 block",
                                      children: "バージョン",
                                    }),
                                    A.jsx("input", {
                                      type: "text",
                                      value: N,
                                      onChange: (tt) => Y(tt.target.value),
                                      className:
                                        "w-full px-3 py-2 border rounded-md text-sm",
                                      placeholder: "1.0.0",
                                    }),
                                  ],
                                }),
                                A.jsxs("div", {
                                  className: "space-y-2",
                                  children: [
                                    A.jsxs("label", {
                                      className:
                                        "flex items-center gap-2 text-sm",
                                      children: [
                                        A.jsx("input", {
                                          type: "checkbox",
                                          checked: G,
                                          onChange: (tt) =>
                                            j(tt.target.checked),
                                          className: "rounded",
                                        }),
                                        "テストコードを含める",
                                      ],
                                    }),
                                    A.jsxs("label", {
                                      className:
                                        "flex items-center gap-2 text-sm",
                                      children: [
                                        A.jsx("input", {
                                          type: "checkbox",
                                          checked: W,
                                          onChange: (tt) =>
                                            I(tt.target.checked),
                                          className: "rounded",
                                        }),
                                        "README を含める",
                                      ],
                                    }),
                                  ],
                                }),
                              ],
                            })
                          : A.jsxs(A.Fragment, {
                              children: [
                                A.jsxs("div", {
                                  children: [
                                    A.jsx("label", {
                                      className:
                                        "text-sm font-medium mb-2 block",
                                      children: "デプロイ先",
                                    }),
                                    A.jsx("div", {
                                      className: "grid grid-cols-2 gap-2",
                                      children: g
                                        .filter(
                                          (tt) => tt.supports_direct_deploy,
                                        )
                                        .map((tt) =>
                                          A.jsxs(
                                            "button",
                                            {
                                              onClick: () => p(tt.id),
                                              className: `p-3 border rounded-lg text-left transition-colors ${m === tt.id ? "border-primary bg-primary/5" : "hover:border-muted-foreground/50"}`,
                                              children: [
                                                A.jsxs("div", {
                                                  className:
                                                    "flex items-center gap-2",
                                                  children: [
                                                    A.jsx("span", {
                                                      className: "text-lg",
                                                      children: tt.icon,
                                                    }),
                                                    A.jsx("span", {
                                                      className:
                                                        "font-medium text-sm",
                                                      children: tt.name,
                                                    }),
                                                  ],
                                                }),
                                                A.jsx("p", {
                                                  className:
                                                    "text-xs text-muted-foreground mt-1",
                                                  children: tt.description,
                                                }),
                                              ],
                                            },
                                            tt.id,
                                          ),
                                        ),
                                    }),
                                  ],
                                }),
                                Object.entries(F).map(([tt, ft]) =>
                                  A.jsxs(
                                    "div",
                                    {
                                      className: "space-y-3",
                                      children: [
                                        A.jsx("h4", {
                                          className:
                                            "text-sm font-medium capitalize",
                                          children:
                                            tt === "credentials"
                                              ? "🔐 認証情報"
                                              : "⚙️ 設定",
                                        }),
                                        ft.map((ht) =>
                                          A.jsxs(
                                            "div",
                                            {
                                              children: [
                                                A.jsxs("label", {
                                                  className:
                                                    "text-sm text-muted-foreground mb-1 block",
                                                  children: [
                                                    ht.label,
                                                    ht.required &&
                                                      A.jsx("span", {
                                                        className:
                                                          "text-destructive ml-1",
                                                        children: "*",
                                                      }),
                                                  ],
                                                }),
                                                $(ht),
                                                ht.description &&
                                                  ht.type !== "boolean" &&
                                                  A.jsx("p", {
                                                    className:
                                                      "text-xs text-muted-foreground mt-1",
                                                    children: ht.description,
                                                  }),
                                              ],
                                            },
                                            ht.name,
                                          ),
                                        ),
                                      ],
                                    },
                                    tt,
                                  ),
                                ),
                                R.length > 0 &&
                                  A.jsxs("div", {
                                    className:
                                      "p-3 bg-muted/50 rounded-lg max-h-40 overflow-auto",
                                    children: [
                                      A.jsx("h4", {
                                        className: "text-xs font-medium mb-2",
                                        children: "デプロイログ",
                                      }),
                                      R.map((tt, ft) =>
                                        A.jsx(
                                          "p",
                                          {
                                            className: "text-xs font-mono",
                                            children: tt,
                                          },
                                          ft,
                                        ),
                                      ),
                                    ],
                                  }),
                                O &&
                                  A.jsxs("div", {
                                    className: `p-3 rounded-md text-sm ${O.status === "error" ? "bg-destructive/10 text-destructive" : "bg-green-50 text-green-700"}`,
                                    children: [
                                      A.jsx("p", {
                                        className: "font-medium",
                                        children:
                                          O.status === "error"
                                            ? "❌ デプロイ失敗"
                                            : "✅ デプロイ完了",
                                      }),
                                      O.url &&
                                        A.jsx("a", {
                                          href: O.url,
                                          target: "_blank",
                                          rel: "noopener noreferrer",
                                          className:
                                            "text-primary hover:underline block mt-1",
                                          children: O.url,
                                        }),
                                      O.error &&
                                        A.jsx("p", {
                                          className: "mt-1",
                                          children: O.error,
                                        }),
                                    ],
                                  }),
                              ],
                            }),
                    }),
                    A.jsxs("div", {
                      className: "space-y-3",
                      children: [
                        A.jsxs("div", {
                          className: "flex items-center justify-between",
                          children: [
                            A.jsx("h3", {
                              className: "font-medium text-sm",
                              children: "生成コードプレビュー",
                            }),
                            A.jsx("button", {
                              onClick: it,
                              disabled: et,
                              className:
                                "px-3 py-1.5 bg-secondary text-secondary-foreground rounded-md text-xs hover:bg-secondary/90 disabled:opacity-50",
                              children: et ? "読み込み中..." : "プレビュー更新",
                            }),
                          ],
                        }),
                        Q
                          ? A.jsxs("div", {
                              className: "border rounded-lg overflow-hidden",
                              children: [
                                A.jsx("div", {
                                  className:
                                    "flex overflow-x-auto bg-muted/50 border-b",
                                  children: Object.keys(Q).map((tt) =>
                                    A.jsx(
                                      "button",
                                      {
                                        onClick: () => st(tt),
                                        className: `px-3 py-2 text-xs whitespace-nowrap border-b-2 transition-colors ${at === tt ? "border-primary bg-background" : "border-transparent hover:bg-muted"}`,
                                        children: tt,
                                      },
                                      tt,
                                    ),
                                  ),
                                }),
                                at &&
                                  Q[at] &&
                                  A.jsxs("div", {
                                    className: "p-3 bg-background",
                                    children: [
                                      A.jsxs("div", {
                                        className:
                                          "flex items-center justify-between text-xs text-muted-foreground mb-2",
                                        children: [
                                          A.jsxs("span", {
                                            children: [Q[at].lines, " 行"],
                                          }),
                                          A.jsxs("span", {
                                            children: [
                                              (Q[at].size / 1024).toFixed(1),
                                              " KB",
                                            ],
                                          }),
                                        ],
                                      }),
                                      A.jsx("pre", {
                                        className:
                                          "text-xs font-mono bg-muted p-3 rounded-md overflow-auto max-h-64",
                                        children: Q[at].content,
                                      }),
                                    ],
                                  }),
                              ],
                            })
                          : A.jsx("div", {
                              className:
                                "border rounded-lg p-8 text-center text-muted-foreground",
                              children: A.jsxs("p", {
                                className: "text-sm",
                                children: [
                                  "「プレビュー更新」をクリックして",
                                  A.jsx("br", {}),
                                  "生成されるコードを確認",
                                ],
                              }),
                            }),
                      ],
                    }),
                  ],
                }),
              }),
              A.jsxs("div", {
                className: "p-4 border-t flex justify-end gap-3",
                children: [
                  A.jsx("button", {
                    onClick: u,
                    className:
                      "px-4 py-2 border rounded-md text-sm hover:bg-muted",
                    children: "キャンセル",
                  }),
                  b === "export"
                    ? A.jsx("button", {
                        onClick: ot,
                        disabled: rt || i.nodes.length === 0,
                        className:
                          "px-4 py-2 bg-primary text-primary-foreground rounded-md text-sm hover:bg-primary/90 disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-2",
                        children: rt
                          ? A.jsxs(A.Fragment, {
                              children: [
                                A.jsx("span", {
                                  className: "animate-spin",
                                  children: "⏳",
                                }),
                                "エクスポート中...",
                              ],
                            })
                          : A.jsxs(A.Fragment, {
                              children: [
                                A.jsx("span", { children: "📦" }),
                                "ZIP ダウンロード",
                              ],
                            }),
                      })
                    : A.jsx("button", {
                        onClick: dt,
                        disabled: L || i.nodes.length === 0,
                        className:
                          "px-4 py-2 bg-primary text-primary-foreground rounded-md text-sm hover:bg-primary/90 disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-2",
                        children: L
                          ? A.jsxs(A.Fragment, {
                              children: [
                                A.jsx("span", {
                                  className: "animate-spin",
                                  children: "⏳",
                                }),
                                "デプロイ中...",
                              ],
                            })
                          : A.jsxs(A.Fragment, {
                              children: [
                                A.jsx("span", { children: "🚀" }),
                                "デプロイ",
                              ],
                            }),
                      }),
                ],
              }),
            ],
          }),
        ],
      })
    : null;
}
function Y_() {
  const { workflow: l, saveWorkflow: u } = Ia(),
    [i, s] = H.useState(!0),
    [o, f] = H.useState(!1),
    [d, g] = H.useState(!1),
    y = async () => {
      g(!0);
      try {
        await u();
      } catch (m) {
        console.error("Save failed:", m);
      } finally {
        g(!1);
      }
    };
  return A.jsxs(zf, {
    children: [
      A.jsxs("div", {
        className:
          "flex flex-col h-screen w-screen overflow-hidden bg-background",
        children: [
          A.jsxs("div", {
            className:
              "h-12 border-b bg-background flex items-center justify-between px-4",
            children: [
              A.jsxs("div", {
                className: "flex items-center gap-4",
                children: [
                  A.jsx("span", {
                    className: "font-bold text-primary",
                    children: "🤖 AgentFlow Studio",
                  }),
                  A.jsx("span", {
                    className: "text-sm text-muted-foreground",
                    children: l.name || "新しいワークフロー",
                  }),
                ],
              }),
              A.jsxs("div", {
                className: "flex items-center gap-2",
                children: [
                  A.jsx("button", {
                    onClick: y,
                    disabled: d,
                    className:
                      "px-3 py-1.5 text-sm border rounded-md hover:bg-muted disabled:opacity-50 flex items-center gap-1",
                    children: d ? "💾 保存中..." : "💾 保存",
                  }),
                  A.jsx("button", {
                    onClick: () => s(!i),
                    className: `px-3 py-1.5 text-sm border rounded-md hover:bg-muted flex items-center gap-1 ${i ? "bg-primary/10 border-primary" : ""}`,
                    children: "▶ Preview",
                  }),
                  A.jsx("button", {
                    onClick: () => f(!0),
                    className:
                      "px-3 py-1.5 text-sm bg-primary text-primary-foreground rounded-md hover:bg-primary/90 flex items-center gap-1",
                    children: "🚀 発行",
                  }),
                ],
              }),
            ],
          }),
          A.jsxs("div", {
            className: "flex flex-1 overflow-hidden",
            children: [
              A.jsx(H_, {}),
              A.jsx("div", {
                className: "flex-1 relative",
                children: A.jsx(R_, {}),
              }),
              A.jsx(j_, {}),
              i && A.jsx(U_, {}),
            ],
          }),
        ],
      }),
      A.jsx(B_, { open: o, onClose: () => f(!1) }),
    ],
  });
}
h1.createRoot(document.getElementById("root")).render(
  A.jsx(J.StrictMode, { children: A.jsx(Y_, {}) }),
);
