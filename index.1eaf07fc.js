var n={};!function(n){function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function a(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(a){return n(r,t,e,u,a)}}}}}))}function i(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(a){return function(i){return n(r,t,e,u,a,i)}}}}}}))}function c(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function f(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function o(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function s(n,r,t,e,u,a){return 5===n.a?n.f(r,t,e,u,a):n(r)(t)(e)(u)(a)}function v(n,r,t,e,u,a,i){return 6===n.a?n.f(r,t,e,u,a,i):n(r)(t)(e)(u)(a)(i)}var b=e((function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e})),l=t((function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,p(t,r)}));function d(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}function h(n,r){for(var t,e=[],u=g(n,r,0,e);u&&(t=e.pop());u=g(t.a,t.b,0,e));return u}function g(n,r,t,e){if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&d(5),!1;if(t>100)return e.push(p(n,r)),!0;for(var u in 0>n.$&&(n=Er(n),r=Er(r)),n)if(!g(n[u],r[u],t+1,e))return!1;return!0}function m(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=m(n.a,r.a))||(t=m(n.b,r.b))?t:m(n.c,r.c);for(;n.b&&r.b&&!(t=m(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var $=t((function(n,r){var t=m(n,r);return 0>t?kr:t?xr:Ar}));function p(n,r){return{a:n,b:r}}function w(n,r,t){return{a:n,b:r,c:t}}function y(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}function j(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=A(n.a,r);n=n.b;for(var e=t;n.b;n=n.b)e=e.b=A(n.a,r);return t}var E={$:0};function A(n,r){return{$:1,a:n,b:r}}var x=t(A);function k(n){for(var r=E,t=n.length;t--;)r=A(n[t],r);return r}var N=t((function(n,r){var t=r%n;return 0===n?d(11):t>0&&0>n||0>t&&n>0?t+n:t})),_=Math.ceil,C=Math.floor,T=Math.round,L=Math.log,S=isNaN,M=t((function(n,r){return n+r})),O=e((function(n,r,t){for(var e=t.length;e--;){var u=t[e],a=t.charCodeAt(e);56320>a||a>57343||(u=t[--e]+u),r=c(n,u,r)}return r})),q=t((function(n,r){return r.split(n)})),I=t((function(n,r){return r.join(n)})),W=e((function(n,r,t){return t.slice(n,r)})),F=t((function(n,r){return 0===r.indexOf(n)}));function K(n){return n+""}function B(n){return{$:2,b:n}}var z=B((function(n){return"number"!=typeof n?en("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?Lr(n):!isFinite(n)||n%1?en("an INT",n):Lr(n)})),H=B((function(n){return"boolean"==typeof n?Lr(n):en("a BOOL",n)})),D=B((function(n){return"number"==typeof n?Lr(n):en("a FLOAT",n)}));B((function(n){return Lr(n)})),B((function(n){return"string"==typeof n?Lr(n):n instanceof String?Lr(n+""):en("a STRING",n)}));var J=t((function(n,r){return{$:6,d:n,b:r}}));function R(n,r){return{$:9,f:n,g:r}}var Y,Q=t((function(n,r){return{$:10,b:r,h:n}})),X=t((function(n,r){return R(n,[r])})),P=e((function(n,r,t){return R(n,[r,t])})),G=u((function(n,r,t,e){return R(n,[r,t,e])})),Z=r(7,Y=function(n,r,t,e,u,a,i){return R(n,[r,t,e,u,a,i])},(function(n){return function(r){return function(t){return function(e){return function(u){return function(a){return function(i){return Y(n,r,t,e,u,a,i)}}}}}}})),U=t((function(n,r){return V(n,r)}));function V(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?Lr(n.c):en("null",r);case 3:return rn(r)?nn(n.b,r,k):en("a LIST",r);case 4:return rn(r)?nn(n.b,r,tn):en("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return en("an OBJECT with a field named `"+t+"`",r);var e=V(n.b,r[t]);return ft(e)?e:Nr(c(Cr,t,e.a));case 7:var u=n.e;return rn(r)?r.length>u?(e=V(n.b,r[u]),ft(e)?e:Nr(c(Tr,u,e.a))):en("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r):en("an ARRAY",r);case 8:if("object"!=typeof r||null===r||rn(r))return en("an OBJECT",r);var a=E;for(var i in r)if(r.hasOwnProperty(i)){if(e=V(n.b,r[i]),!ft(e))return Nr(c(Cr,i,e.a));a=A(p(i,e.a),a)}return Lr(Hr(a));case 9:for(var f=n.f,o=n.g,s=0;o.length>s;s++){if(e=V(o[s],r),!ft(e))return e;f=f(e.a)}return Lr(f);case 10:return e=V(n.b,r),ft(e)?V(n.h(e.a),r):e;case 11:for(var v=E,b=n.g;b.b;b=b.b){if(e=V(b.a,r),ft(e))return e;v=A(e.a,v)}return Nr(Sr(Hr(v)));case 1:return Nr(c(_r,n.a,r));case 0:return Lr(n.a)}}function nn(n,r,t){for(var e=r.length,u=Array(e),a=0;e>a;a++){var i=V(n,r[a]);if(!ft(i))return Nr(c(Tr,a,i.a));u[a]=i.a}return Lr(t(u))}function rn(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function tn(n){return c(ct,n.length,(function(r){return n[r]}))}function en(n,r){return Nr(c(_r,"Expecting "+n,r))}function un(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return un(n.b,r.b);case 6:return n.d===r.d&&un(n.b,r.b);case 7:return n.e===r.e&&un(n.b,r.b);case 9:return n.f===r.f&&an(n.g,r.g);case 10:return n.h===r.h&&un(n.b,r.b);case 11:return an(n.g,r.g)}}function an(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!un(n[e],r[e]))return!1;return!0}function cn(n){return n}function fn(n){return{$:0,a:n}}function on(n){return{$:2,b:n,c:null}}var sn=t((function(n,r){return{$:3,b:n,d:r}})),vn=t((function(n,r){return{$:4,b:n,d:r}})),bn=0;function ln(n){var r={$:0,e:bn++,f:n,g:null,h:[]};return pn(r),r}function dn(n){return on((function(r){r(fn(ln(n)))}))}function hn(n,r){n.h.push(r),pn(n)}var gn=t((function(n,r){return on((function(t){hn(n,r),t(fn(0))}))})),mn=!1,$n=[];function pn(n){if($n.push(n),!mn){for(mn=!0;n=$n.shift();)wn(n);mn=!1}}function wn(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,pn(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}var yn={};function jn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function En(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,a=n.e,i=n.f;return t.h=ln(c(sn,(function n(r){return c(sn,n,{$:5,b:function(n){var c=n.a;return 0===n.$?f(u,t,c,r):a&&i?o(e,t,c.i,c.j,r):f(e,t,a?c.i:c.j,r)}})}),n.b))}var An=t((function(n,r){return on((function(t){n.g(r),t(fn(0))}))})),xn=t((function(n,r){return c(gn,n.h,{$:0,a:r})}));function kn(n){return function(r){return{$:1,k:n,l:r}}}function Nn(n){return{$:2,m:n}}var _n,Cn=[],Tn=!1;function Ln(n,r,t){if(Cn.push({p:n,q:r,r:t}),!Tn){Tn=!0;for(var e;e=Cn.shift();)Sn(e.p,e.q,e.r);Tn=!1}}function Sn(n,r,t){var e={};for(var u in Mn(!0,r,e,null),Mn(!1,t,e,null),n)hn(n[u],{$:"fx",a:e[u]||{i:E,j:E}})}function Mn(n,r,t,e){switch(r.$){case 1:var u=r.k,a=(f=n,o=u,s=e,v=r.l,c(f?yn[o].e:yn[o].f,(function(n){for(var r=s;r;r=r.t)n=r.s(n);return n}),v));return void(t[u]=function(n,r,t){return t=t||{i:E,j:E},n?t.i=A(r,t.i):t.j=A(r,t.j),t}(n,a,t[u]));case 2:for(var i=r.m;i.b;i=i.b)Mn(n,i.a,t,e);return;case 3:return void Mn(n,r.o,t,{s:r.n,t:e})}var f,o,s,v}var On="undefined"!=typeof document?document:{};function qn(n,r){n.appendChild(r)}function In(n){return{$:0,a:n}}var Wn=t((function(n,r){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b||0,u.push(i)}return a+=u.length,{$:1,c:r,d:Dn(t),e:u,f:n,b:a}}))}))(void 0);t((function(n,r){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b.b||0,u.push(i)}return a+=u.length,{$:2,c:r,d:Dn(t),e:u,f:n,b:a}}))}))(void 0);var Fn,Kn=t((function(n,r){return{$:"a0",n:n,o:r}})),Bn=t((function(n,r){return{$:"a1",n:n,o:r}})),zn=t((function(n,r){return{$:"a2",n:n,o:r}})),Hn=t((function(n,r){return{$:"a3",n:n,o:r}}));function Dn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,a=t.o;if("a2"!==e){var i=r[e]||(r[e]={});"a3"===e&&"class"===u?Jn(i,u,a):i[u]=a}else"className"===u?Jn(r,u,a):r[u]=a}return r}function Jn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function Rn(n,r){var t=n.$;if(5===t)return Rn(n.k||(n.k=n.m()),r);if(0===t)return On.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:r};return(i=Rn(e,a)).elm_event_node_ref=a,i}if(3===t)return Yn(i=n.h(n.g),r,n.d),i;var i=n.f?On.createElementNS(n.f,n.c):On.createElement(n.c);_n&&"a"==n.c&&i.addEventListener("click",_n(i)),Yn(i,r,n.d);for(var c=n.e,f=0;c.length>f;f++)qn(i,Rn(1===t?c[f]:c[f].b,r));return i}function Yn(n,r,t){for(var e in t){var u=t[e];"a1"===e?Qn(n,u):"a0"===e?Gn(n,r,u):"a3"===e?Xn(n,u):"a4"===e?Pn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function Qn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Xn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Pn(n,r){for(var t in r){var e=r[t],u=e.f,a=e.o;void 0!==a?n.setAttributeNS(u,t,a):n.removeAttributeNS(u,t)}}function Gn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var a=t[u],i=e[u];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}n.removeEventListener(u,i)}i=Zn(r,a),n.addEventListener(u,i,Fn&&{passive:2>bt(a)}),e[u]=i}else n.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Fn=!0}}))}catch(n){}function Zn(n,r){function t(r){var e=t.q,u=V(e.a,r);if(ft(u)){for(var a,i=bt(e),c=u.a,f=i?3>i?c.a:c.w:c,o=1==i?c.b:3==i&&c.av,s=(o&&r.stopPropagation(),(2==i?c.b:3==i&&c.ar)&&r.preventDefault(),n);a=s.j;){if("function"==typeof a)f=a(f);else for(var v=a.length;v--;)f=a[v](f);s=s.p}s(f,o)}}return t.q=r,t}function Un(n,r){return n.$==r.$&&un(n.a,r.a)}function Vn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function nr(n,r,t,e){if(n!==r){var u=n.$,a=r.$;if(u!==a){if(1!==u||2!==a)return void Vn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),a=1}switch(a){case 5:for(var i=n.l,c=r.l,f=i.length,o=f===c.length;o&&f--;)o=i[f]===c[f];if(o)return void(r.k=n.k);r.k=r.m();var s=[];return nr(n.k,r.k,s,0),void(s.length>0&&Vn(t,1,e,s));case 4:for(var v=n.j,b=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof v?v=[v,d.j]:v.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return l&&v.length!==b.length?void Vn(t,0,e,r):((l?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(v,b):v===b)||Vn(t,2,e,b),void nr(d,h,t,e+1));case 0:return void(n.a!==r.a&&Vn(t,3,e,r.a));case 1:return void rr(n,r,t,e,er);case 2:return void rr(n,r,t,e,ur);case 3:if(n.h!==r.h)return void Vn(t,0,e,r);var g=tr(n.d,r.d);g&&Vn(t,4,e,g);var m=r.i(n.g,r.g);return void(m&&Vn(t,5,e,m))}}}function rr(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var a=tr(n.d,r.d);a&&Vn(t,4,e,a),u(n,r,t,e)}else Vn(t,0,e,r)}function tr(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var a=n[u],i=r[u];a===i&&"value"!==u&&"checked"!==u||"a0"===t&&Un(a,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var c=tr(n[u],r[u]||{},u);c&&((e=e||{})[u]=c)}for(var f in r)f in n||((e=e||{})[f]=r[f]);return e}function er(n,r,t,e){var u=n.e,a=r.e,i=u.length,c=a.length;i>c?Vn(t,6,e,{v:c,i:i-c}):c>i&&Vn(t,7,e,{v:i,e:a});for(var f=c>i?i:c,o=0;f>o;o++){var s=u[o];nr(s,a[o],t,++e),e+=s.b||0}}function ur(n,r,t,e){for(var u=[],a={},i=[],c=n.e,f=r.e,o=c.length,s=f.length,v=0,b=0,l=e;o>v&&s>b;){var d=(k=c[v]).a,h=(N=f[b]).a,g=k.b,m=N.b,$=void 0,p=void 0;if(d!==h){var w=c[v+1],y=f[b+1];if(w){var j=w.a,E=w.b;p=h===j}if(y){var A=y.a,x=y.b;$=d===A}if($&&p)nr(g,x,u,++l),ir(a,u,d,m,b,i),l+=g.b||0,cr(a,u,d,E,++l),l+=E.b||0,v+=2,b+=2;else if($)l++,ir(a,u,h,m,b,i),nr(g,x,u,l),l+=g.b||0,v+=1,b+=2;else if(p)cr(a,u,d,g,++l),l+=g.b||0,nr(E,m,u,++l),l+=E.b||0,v+=2,b+=1;else{if(!w||j!==A)break;cr(a,u,d,g,++l),ir(a,u,h,m,b,i),l+=g.b||0,nr(E,x,u,++l),l+=E.b||0,v+=2,b+=2}}else nr(g,m,u,++l),l+=g.b||0,v++,b++}for(;o>v;){var k;l++,cr(a,u,(k=c[v]).a,g=k.b,l),l+=g.b||0,v++}for(;s>b;){var N,_=_||[];ir(a,u,(N=f[b]).a,N.b,void 0,_),b++}(u.length>0||i.length>0||_)&&Vn(t,8,e,{w:u,x:i,y:_})}var ar="_elmW6BL";function ir(n,r,t,e,u,a){var i=n[t];if(!i)return a.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(n[t]=i);if(1===i.c){a.push({r:u,A:i}),i.c=2;var c=[];return nr(i.z,e,c,i.r),i.r=u,void(i.s.s={w:c,A:i})}ir(n,r,t+ar,e,u,a)}function cr(n,r,t,e,u){var a=n[t];if(a){if(0===a.c){a.c=2;var i=[];return nr(e,a.z,i,u),void Vn(r,9,u,{w:i,A:a})}cr(n,r,t+ar,e,u)}else{var c=Vn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:c}}}function fr(n,r,t,e){or(n,r,t,0,0,r.b,e)}function or(n,r,t,e,u,a,i){for(var c=t[e],f=c.r;f===u;){var o=c.$;if(1===o)fr(n,r.k,c.s,i);else if(8===o)c.t=n,c.u=i,(s=c.s.w).length>0&&or(n,r,s,0,u,a,i);else if(9===o){c.t=n,c.u=i;var s,v=c.s;v&&(v.A.s=n,(s=v.w).length>0&&or(n,r,s,0,u,a,i))}else c.t=n,c.u=i;if(!(c=t[++e])||(f=c.r)>a)return e}var b=r.$;if(4===b){for(var l=r.k;4===l.$;)l=l.k;return or(n,l,t,e,u+1,a,n.elm_event_node_ref)}for(var d=r.e,h=n.childNodes,g=0;d.length>g;g++){u++;var m=1===b?d[g]:d[g].b,$=u+(m.b||0);if(!(u>f||f>$||(c=t[e=or(h[g],m,t,e,u,$,i)])&&(f=c.r)<=a))return e;u=$}return e}function sr(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,a=vr(u,e);u===n&&(n=a)}return n}function vr(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=Rn(r,t);return u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref),e&&u!==n&&e.replaceChild(u,n),u}(n,r.s,r.u);case 4:return Yn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return sr(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,a=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(Rn(u[e],r.u),a);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var i=t.A;return void 0!==i.r&&n.parentNode.removeChild(n),i.s=sr(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(n){for(var t=On.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;qn(t,2===u.c?u.s:Rn(u.z,r.u))}return t}}(t.y,r);n=sr(n,t.w);for(var u=t.x,a=0;u.length>a;a++){var i=u[a],c=i.A,f=2===c.c?c.s:Rn(c.z,r.u);n.insertBefore(f,n.childNodes[i.r])}return e&&qn(n,e),n}(n,r);case 5:return r.s(n);default:d(10)}}function br(n){if(3===n.nodeType)return In(n.textContent);if(1!==n.nodeType)return In("");for(var r=E,t=n.attributes,e=t.length;e--;){var u=t[e];r=A(c(Hn,u.name,u.value),r)}var a=n.tagName.toLowerCase(),i=E,o=n.childNodes;for(e=o.length;e--;)i=A(br(o[e]),i);return f(Wn,a,r,i)}var lr=u((function(n,r,t,e){return function(r,t,e,u,a,i){var f=c(U,r,t?t.flags:void 0);ft(f)||d(2);var o={},s=e(f.a),v=s.a,b=function(r,t){var e=n.at&&n.at(r),u=n.b0,a=On.title,i=On.body,c=br(i);return function(n,r){r(n);var t=0;function e(){t=1===t?0:(dr(e),r(n),1)}return function(u,a){n=u,a?(r(n),2===t&&(t=1)):(0===t&&dr(e),t=2)}}(t,(function(n){_n=e;var t=u(n),f=Wn("body")(E)(t.bm),o=function(n,r){var t=[];return nr(n,r,t,0),t}(c,f);i=function(n,r,t,e){return 0===t.length?n:(fr(n,r,t,e),sr(n,t))}(i,c,o,r),c=f,_n=0,a!==t.E&&(On.title=a=t.E)}))}(h,v),l=function(n,r){var t;for(var e in yn){var u=yn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=En(u,r)}return t}(o,h);function h(n,r){var t=c(u,n,v);b(v=t.a,r),Ln(o,t.b,a(v))}return Ln(o,s.b,a(v)),l?{ports:l}:{}}(r,e,n.bE,n.b$,n.bW)})),dr="undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)},hr={addEventListener:function(){},removeEventListener:function(){}},gr="undefined"!=typeof document?document:hr,mr="undefined"!=typeof window?window:hr,$r=e((function(n,r,t){return dn(on((function(){function e(n){ln(t(n))}return n.addEventListener(r,e,Fn&&{passive:!0}),function(){n.removeEventListener(r,e)}})))})),pr=t((function(n,r){var t=V(n,r);return ft(t)?Mr(t.a):Or})),wr=t((function(n,r){return on((function(){var t=setInterval((function(){ln(r)}),n);return function(){clearInterval(t)}}))})),yr=x,jr=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,a=f(n,t.b,t.c,f(jr,n,r,t.e));n=u,r=a,t=e}})),Er=function(n){return f(jr,e((function(n,r,t){return c(yr,p(n,r),t)})),E,n)},Ar=1,xr=2,kr=0,Nr=function(n){return{$:1,a:n}},_r=t((function(n,r){return{$:3,a:n,b:r}})),Cr=t((function(n,r){return{$:0,a:n,b:r}})),Tr=t((function(n,r){return{$:1,a:n,b:r}})),Lr=function(n){return{$:0,a:n}},Sr=function(n){return{$:2,a:n}},Mr=function(n){return{$:0,a:n}},Or={$:1},qr=K,Ir=t((function(n,r){return c(I,n,function(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}(r))})),Wr=t((function(n,r){return k(c(q,n,r))})),Fr=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,a=c(n,t.a,r);n=u,r=a,t=e}})),Kr=e((function(n,r,t){for(;;){if(m(n,r)>=1)return t;var e=n,u=r-1,a=c(yr,r,t);n=e,r=u,t=a}})),Br=t((function(n,r){return f(Kr,n,r,E)})),zr=function(n){var r=n.charCodeAt(0);return 55296>r||r>56319?r:1024*(r-55296)+n.charCodeAt(1)-56320+65536},Hr=function(n){return f(Fr,yr,E,n)},Dr=function(n){var r=n.charCodeAt(0);return isNaN(r)?Or:Mr(55296>r||r>56319?p(n[0],n.slice(1)):p(n[0]+n[1],n.slice(2)))},Jr=32,Rr=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),Yr=[],Qr=_,Xr=t((function(n,r){return L(r)/L(n)})),Pr=Qr(c(Xr,2,Jr)),Gr=o(Rr,0,Pr,Yr,Yr),Zr=b,Ur=C,Vr=function(n){return n.length},nt=t((function(n,r){return m(n,r)>0?n:r})),rt=l,tt=t((function(n,r){for(;;){var t=c(rt,Jr,n),e=t.b,u=c(yr,{$:0,a:t.a},r);if(!e.b)return Hr(u);n=e,r=u}})),et=function(n){return n.a},ut=t((function(n,r){for(;;){var t=Qr(r/Jr);if(1===t)return c(rt,Jr,n).a;n=c(tt,n,E),r=t}})),at=t((function(n,r){if(r.e){var t=r.e*Jr,e=Ur(c(Xr,Jr,t-1)),u=n?Hr(r.j):r.j,a=c(ut,u,r.e);return o(Rr,Vr(r.h)+t,c(nt,5,e*Pr),a,r.h)}return o(Rr,Vr(r.h),Pr,Yr,r.h)})),it=a((function(n,r,t,e,u){for(;;){if(0>r)return c(at,!1,{j:e,e:t/Jr|0,h:u});var a={$:1,a:f(Zr,Jr,r,n)};r-=Jr,e=c(yr,a,e)}})),ct=t((function(n,r){if(n>0){var t=n%Jr;return s(it,r,n-t-Jr,n,E,f(Zr,t,n-t,r))}return Gr})),ft=function(n){return!n.$},ot=Q,st=X,vt=P,bt=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},lt=function(n){return n},dt=lt,ht=function(n){return n.length},gt=W,mt=t((function(n,r){return 1>n?r:f(gt,n,ht(r),r)})),$t=F,pt=fn,wt=pt(0),yt=u((function(n,r,t,e){if(e.b){var u=e.a,a=e.b;if(a.b){var i=a.a,s=a.b;if(s.b){var v=s.a,b=s.b;if(b.b){var l=b.b;return c(n,u,c(n,i,c(n,v,c(n,b.a,t>500?f(Fr,n,r,Hr(l)):o(yt,n,r,t+1,l)))))}return c(n,u,c(n,i,c(n,v,r)))}return c(n,u,c(n,i,r))}return c(n,u,r)}return r})),jt=e((function(n,r,t){return o(yt,n,r,0,t)})),Et=t((function(n,r){return f(jt,t((function(r,t){return c(yr,n(r),t)})),E,r)})),At=sn,xt=t((function(n,r){return c(At,(function(r){return pt(n(r))}),r)})),kt=e((function(n,r,t){return c(At,(function(r){return c(At,(function(t){return pt(c(n,r,t))}),t)}),r)})),Nt=function(n){return f(jt,kt(yr),pt(E),n)},_t=An,Ct=t((function(n,r){var t=r;return dn(c(At,_t(n),t))}));yn.Task=jn(wt,e((function(n,r){return c(xt,(function(){return 0}),Nt(c(Et,Ct(n),r)))})),e((function(){return pt(0)})),t((function(n,r){return c(xt,n,r)})));var Tt,Lt=kn("Task"),St=t((function(n,r){return Lt(c(xt,n,r))})),Mt=lr,Ot=J,qt={$:4},It=function(n){return{$:0,a:n}},Wt=Nn,Ft=t((function(n,r){return{$:0,a:n,b:r}})),Kt=Ft,Bt=on((function(n){n(fn(c(Kt,-(new Date).getTimezoneOffset(),E)))})),zt=lt,Ht=c(Ft,0,E),Dt=z,Jt=function(n){return{$:1,a:n}},Rt=Nn,Yt=t((function(n,r){return{$:0,a:n,b:r}})),Qt=t((function(n,r){return{a1:r,bc:n}})),Xt={$:-2},Pt=Xt,Gt=pt(c(Qt,Pt,Pt)),Zt=$,Ut=t((function(n,r){n:for(;;){if(-2===r.$)return Or;var t=r.c,e=r.d,u=r.e;switch(c(Zt,n,r.b)){case 0:r=e;continue n;case 1:return Mr(t);default:r=u;continue n}}})),Vt=a((function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}})),ne=a((function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return s(Vt,n,r,t,e,u);var a=e.d;return i=e.e,s(Vt,0,e.b,e.c,s(Vt,1,a.b,a.c,a.d,a.e),s(Vt,1,r,t,i,u))}var i,c=u.b,f=u.c,o=u.d,v=u.e;return-1!==e.$||e.a?s(Vt,n,c,f,s(Vt,0,r,t,e,o),v):s(Vt,0,r,t,s(Vt,1,e.b,e.c,e.d,i=e.e),s(Vt,1,c,f,o,v))})),re=e((function(n,r,t){if(-2===t.$)return s(Vt,0,n,r,Xt,Xt);var e=t.a,u=t.b,a=t.c,i=t.d,o=t.e;switch(c(Zt,n,u)){case 0:return s(ne,e,u,a,f(re,n,r,i),o);case 1:return s(Vt,e,u,r,i,o);default:return s(ne,e,u,a,i,f(re,n,r,o))}})),te=e((function(n,r,t){var e=f(re,n,r,t);return-1!==e.$||e.a?e:s(Vt,1,e.b,e.c,e.d,e.e)})),ee=t((function(n,r){var t=n.a,e=n.b,u=c(Ut,t,r);return f(te,t,1===u.$?k([e]):c(yr,e,u.a),r)})),ue=function(n){return on((function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(fn(0))}))},ae=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,a=f(n,t.b,t.c,f(ae,n,r,t.d));n=u,r=a,t=e}})),ie=i((function(n,r,u,a,i,c){var s=f(ae,e((function(t,e,a){n:for(;;){var i=a.a,c=a.b;if(i.b){var s=i.a,v=s.a,b=s.b,l=i.b;if(0>m(v,t)){a=p(l,f(n,v,b,c));continue n}return m(v,t)>0?p(i,f(u,t,e,c)):p(l,o(r,v,b,e,c))}return p(i,f(u,t,e,c))}})),p(Er(a),c),i),v=s.a,b=s.b;return f(Fr,t((function(r,t){return f(n,r.a,r.b,t)})),b,v)})),ce=xn,fe=wr,oe=dn,se=e((function(n,r,t){if(r.b){var e=r.a,u=r.b,a=oe(c(fe,e,c(ce,n,e)));return c(At,(function(r){return f(se,n,u,f(te,e,r,t))}),a)}return pt(t)})),ve=e((function(n,r,t){var a=t.a1,i=e((function(n,r,t){var e=t.c;return w(t.a,t.b,c(At,(function(){return e}),ue(r)))})),o=f(Fr,ee,Pt,r),s=v(ie,e((function(n,r,t){var e=t.b,u=t.c;return w(c(yr,n,t.a),e,u)})),u((function(n,r,t,e){var u=e.c;return w(e.a,f(te,n,t,e.b),u)})),i,o,a,w(E,Pt,pt(0))),b=s.a,l=s.b;return c(At,(function(n){return pt(c(Qt,o,n))}),c(At,(function(){return f(se,n,b,l)}),s.c))})),be=(Tt=zt,on((function(n){n(fn(Tt(Date.now())))}))),le=e((function(n,r,t){var e=c(Ut,r,t.bc);if(1===e.$)return pt(t);var u=e.a;return c(At,(function(){return pt(t)}),c(At,(function(r){return Nt(c(Et,(function(t){return c(_t,n,t(r))}),u))}),be))})),de=e((function(n,r,t){return n(r(t))}));yn.Time=jn(Gt,ve,le,0,t((function(n,r){return c(Yt,r.a,c(de,n,r.b))})));var he=kn("Time"),ge=t((function(n,r){return he(c(Yt,n,r))})),me=e((function(n,r,t){return{$:0,a:n,b:r,c:t}})),$e=t((function(n,r){return{aZ:r,bb:n}})),pe=pt(c($e,E,Pt)),we=function(n){return p(j(n.a?"w_":"d_",n.b),n)},ye=function(n){return f(Fr,t((function(n,r){return f(te,n.a,n.b,r)})),Pt,n)},je=t((function(n,r){return{aF:r,aQ:n}})),Ee=e((function(n,r,t){return c(xt,(function(n){return p(r,n)}),f($r,t.a?mr:gr,t.b,(function(t){return c(ce,n,c(je,r,t))})))})),Ae=t((function(n,r){return f(ae,te,r,n)})),xe=e((function(n,r,t){var a=e((function(r,t,e){var u=e.c;return w(e.a,e.b,c(yr,f(Ee,n,r,t),u))})),i=e((function(n,r,t){var e=t.b,u=t.c;return w(c(yr,r,t.a),e,u)})),o=u((function(n,r,t,e){var u=e.c;return w(e.a,f(te,n,r,e.b),u)})),s=c(Et,we,r),b=v(ie,i,o,a,t.aZ,ye(s),w(E,Pt,E)),l=b.b,d=b.c;return c(At,(function(n){return pt(c($e,s,c(Ae,l,ye(n))))}),c(At,(function(){return Nt(d)}),Nt(c(Et,ue,b.a))))})),ke=e((function(n,r,t){var e=n(r);return e.$?t:c(yr,e.a,t)})),Ne=t((function(n,r){return f(jt,ke(n),E,r)}));yn["Browser.Events"]=jn(pe,xe,e((function(n,r,t){var e=r.aQ,u=r.aF,a=c(Ne,(function(n){var r=n.b.c;return h(n.a,e)?c(pr,r,u):Or}),t.bb);return c(At,(function(){return pt(t)}),Nt(c(Et,_t(n),a)))})),0,t((function(n,r){return f(me,r.a,r.b,c(st,n,r.c))})));var _e,Ce=kn("Browser.Events"),Te=e((function(n,r,t){return Ce(f(me,n,r,t))})),Le=N,Se=function(n){return!c(Le,4,n)&&!!c(Le,100,n)||!c(Le,400,n)},Me=t((function(n,r){var t=Se(n)?1:0;switch(r){case 0:return 0;case 1:return 31;case 2:return 59+t;case 3:return 90+t;case 4:return 120+t;case 5:return 151+t;case 6:return 181+t;case 7:return 212+t;case 8:return 243+t;case 9:return 273+t;case 10:return 304+t;default:return 334+t}})),Oe=t((function(n,r){return Ur(n/r)})),qe=function(n){var r=n-1;return 365*r+(c(Oe,r,4)-c(Oe,r,100)+c(Oe,r,400))},Ie=t((function(n,r){switch(r){case 0:case 2:case 4:case 6:case 7:case 9:default:return 31;case 1:return Se(n)?29:28;case 3:case 5:case 8:case 10:return 30}})),We=t((function(n,r){return 0>m(n,r)?n:r})),Fe=function(n){switch(n){case 0:return 1;case 1:return 2;case 2:return 3;case 3:return 4;case 4:return 5;case 5:return 6;case 6:return 7;case 7:return 8;case 8:return 9;case 9:return 10;case 10:return 11;default:return 12}},Ke=function(n){switch(c(nt,1,n)){case 1:return 0;case 2:return 1;case 3:return 2;case 4:return 3;case 5:return 4;case 6:return 5;case 7:return 6;case 8:return 7;case 9:return 8;case 10:return 9;case 11:return 10;default:return 11}},Be=e((function(n,r,t){for(;;){var e=c(Ie,n,r),u=Fe(r);if(u>=12||0>=m(t,e))return{aE:t,aS:r,bj:n};r=Ke(u+1),t-=e}})),ze=t((function(n,r){return p(c(Oe,n,r),c(Le,r,n))})),He=function(n){var r=c(ze,n,146097),t=r.a,e=c(ze,r.b,36524),u=e.a,a=c(ze,e.b,1461),i=a.a,f=c(ze,a.b,365);return 400*t+100*u+4*i+f.a+(f.b?1:0)},De=function(n){var r=function(n){var r=n,t=He(r);return{ap:r-qe(t),bj:t}}(n);return f(Be,r.bj,0,r.ap)},Je=e((function(n,r,t){var e=t;switch(n){case 0:return f(Je,1,12*r,e);case 1:var u=De(e),a=12*(u.bj-1)+(Fe(u.aS)-1)+r,i=Ke(c(Le,12,a)+1),o=c(Oe,a,12)+1;return qe(o)+c(Me,o,i)+c(We,u.aE,c(Ie,o,i));case 2:return e+7*r;default:return e+r}})),Re=e((function(n,r,t){return 0>m(t,n)?n:m(t,r)>0?r:t})),Ye=e((function(n,r,t){return qe(n)+c(Me,n,r)+f(Re,1,c(Ie,n,r),t)})),Qe=t((function(n,r){return Ur(n/r)})),Xe=e((function(n,r,t){for(;;){if(!t.b)return r+n;var e=t.a,u=t.b;if(0>m(e.M,r))return r+e.b;t=u}})),Pe=t((function(n,r){var t=n.b;return f(Xe,n.a,c(Qe,r,6e4),t)})),Ge=function(n){var r=c(Qe,n,1440)+719468,t=(0>r?r-146096:r)/146097|0,e=r-146097*t,u=(e-(e/1460|0)+(e/36524|0)-(e/146096|0))/365|0,a=e-(365*u+(u/4|0)-(u/100|0)),i=(5*a+2)/153|0,f=i+(10>i?3:-9);return{aE:a-((153*i+2)/5|0)+1,aS:f,bj:u+400*t+(f>2?0:1)}},Ze=t((function(n,r){return Ge(c(Pe,n,r)).aE})),Ue=t((function(n,r){switch(Ge(c(Pe,n,r)).aS){case 1:return 0;case 2:return 1;case 3:return 2;case 4:return 3;case 5:return 4;case 6:return 5;case 7:return 6;case 8:return 7;case 9:return 8;case 10:return 9;case 11:return 10;default:return 11}})),Ve=t((function(n,r){return Ge(c(Pe,n,r)).bj})),nu=t((function(n,r){return f(Ye,c(Ve,n,r),c(Ue,n,r),c(Ze,n,r))})),ru=function(n){return 864e5*(n-719163)},tu=u((function(n,r,t,e){return 36e5*n+6e4*r+1e3*t+e})),eu=t((function(n,r){return c(Le,24,c(Qe,c(Pe,n,r),60))})),uu=t((function(n,r){return c(Le,1e3,r)})),au=t((function(n,r){return c(Le,60,c(Pe,n,r))})),iu=t((function(n,r){return c(Le,60,c(Qe,r,1e3))})),cu=t((function(n,r){return o(tu,c(eu,n,r),c(au,n,r),c(iu,n,r),c(uu,n,r))})),fu=t((function(n,r){var t=r;return(ru(c(nu,n,r))+c(cu,n,r)-t)/6e4|0})),ou=e((function(n,r,t){var e=ru(r)+t,u=c(fu,n,zt(e)),a=zt(e-6e4*u),i=c(fu,n,a);if(h(u,i))return a;var f=zt(e-6e4*i);return h(i,c(fu,n,f))?f:a})),su=u((function(n,r,t,e){n:for(;;)switch(n){case 15:return zt(e+r);case 14:n=15,r*=1e3;continue n;case 13:n=15,r*=6e4;continue n;case 12:n=15,r*=36e5;continue n;case 11:return f(ou,t,f(Je,3,r,c(nu,t,e)),c(cu,t,e));case 2:return f(ou,t,f(Je,1,r,c(nu,t,e)),c(cu,t,e));case 0:n=2,r*=12;continue n;case 1:n=2,r*=3;continue n;default:n=11,r*=7;continue n}})),vu=t((function(n,r){return c(Le,7,(c(Le,7,r)||7)+7-function(n){switch(n){case 0:return 1;case 1:return 2;case 2:return 3;case 3:return 4;case 4:return 5;case 5:return 6;default:return 7}}(n))})),bu=t((function(n,r){return qe(n)+c(Me,n,r)+1})),lu=e((function(n,r,t){return r(n(t))})),du=c(lu,De,(function(n){return n.aS})),hu=c(lu,du,(function(n){return(Fe(n)+2)/3|0})),gu=t((function(n,r){var t,e,u=r;switch(n){case 0:return e=He(r),qe(e)+1;case 1:return c(bu,He(r),(t=hu(r),Ke(3*t-2)));case 2:return c(bu,He(r),du(r));case 3:case 4:return u-c(vu,0,r);case 5:return u-c(vu,1,r);case 6:return u-c(vu,2,r);case 7:return u-c(vu,3,r);case 8:return u-c(vu,4,r);case 9:return u-c(vu,5,r);case 10:return u-c(vu,6,r);default:return r}})),mu=e((function(n,r,t){return f(ou,r,c(gu,n,c(nu,r,t)),0)})),$u=e((function(n,r,t){switch(n){case 15:return t;case 14:return f(ou,r,c(nu,r,t),o(tu,c(eu,r,t),c(au,r,t),c(iu,r,t),0));case 13:return f(ou,r,c(nu,r,t),o(tu,c(eu,r,t),c(au,r,t),0,0));case 12:return f(ou,r,c(nu,r,t),o(tu,c(eu,r,t),0,0,0));case 11:return f(mu,11,r,t);case 2:return f(mu,2,r,t);case 0:return f(mu,0,r,t);case 1:return f(mu,1,r,t);case 3:return f(mu,3,r,t);case 4:return f(mu,4,r,t);case 5:return f(mu,5,r,t);case 6:return f(mu,6,r,t);case 7:return f(mu,7,r,t);case 8:return f(mu,8,r,t);case 9:return f(mu,9,r,t);default:return f(mu,10,r,t)}})),pu=t((function(n,r){var t=r.g,e=r.i,u=r.as,a=r.am;return f(ou,n,f(Ye,r.bj,r.aS,r.aE),o(tu,f(Re,0,23,t),f(Re,0,59,e),f(Re,0,59,u),f(Re,0,999,a)))})),wu=t((function(n,r){return{aE:c(Ze,n,r),g:c(eu,n,r),am:c(uu,n,r),i:c(au,n,r),aS:c(Ue,n,r),as:c(iu,n,r),bj:c(Ve,n,r)}})),yu=t((function(n,r){var t=c(wu,n,f($u,11,n,r));return k([{I:c(pu,n,y(t,{g:4,i:15})),J:0,M:c(pu,n,y(t,{g:0,i:30})),E:"Impossible state impossible",N:1},{I:c(pu,n,y(t,{g:12,i:30})),J:2,M:c(pu,n,y(t,{g:8,i:15})),E:"Diablo Swing Orchestra",N:3},{I:c(pu,n,y(t,{g:11,i:45})),J:1,M:c(pu,n,y(t,{g:9})),E:"Titanic, a react story",N:2},{I:c(pu,n,y(t,{g:18,i:55})),J:1,M:c(pu,n,y(t,{g:14,i:33})),E:"F# was not that sharp...",N:1}])})),ju=function(n){return function(n,r){return on((function(r){dr((function(){var t=document.getElementById(n);r(t?fn(function(n){var r,t,e=n.getBoundingClientRect(),u=mr.pageXOffset,a=mr.pageYOffset;return{a8:(r=gr.body,t=gr.documentElement,{af:Math.max(r.scrollWidth,r.offsetWidth,t.scrollWidth,t.offsetWidth,t.clientWidth),aK:Math.max(r.scrollHeight,r.offsetHeight,t.scrollHeight,t.offsetHeight,t.clientHeight)}),bf:{b3:u,bi:a,af:gr.documentElement.clientWidth,aK:gr.documentElement.clientHeight},by:{b3:u+e.left,bi:a+e.top,af:e.width,aK:e.height}}}(t)):{$:1,a:dt(n)})}))}))}(n)},Eu=Wt(E),Au=vn,xu=T,ku=t((function(n,r){switch(n.$){case 3:return p(r,Eu);case 4:return p(r,c(St,lt,c(Au,(function(){return c(xt,(function(){return qt}),function(n){return on((function(r){var t=setTimeout((function(){r(fn(0))}),n);return function(){clearTimeout(t)}}))}(10))}),c(xt,(function(n){var r=n.by;return{$:5,a:{af:r.af,ax:r.b3}}}),ju("timeline")))));case 0:return p(y(r,{ac:c(yu,e=n.a,r.K),A:e}),Eu);case 1:return p(y(r,{K:t=n.a}),Eu);case 2:var t=r.K,e=r.A,u=r.ae,a=xu(24*f(Re,0,1,(n.a-u.ax)/u.af)*60);return p(y(r,{W:o(su,13,a,r.A,f($u,11,e,t))}),Eu);case 5:return p(y(r,{ae:n.a}),Eu);default:return p(y(r,{Q:n.a}),Eu)}})),Nu=cn,_u=t((function(n,r){return c(zn,n,Nu(r))}))("checked"),Cu=cn,Tu=t((function(n,r){return c(zn,n,Cu(r))})),Lu=Tu("className"),Su=Wn("div"),Mu=Wn("input"),Ou=Wn("label"),qu=Wn("main"),Iu=Wn("nav"),Wu=function(n){return 0>n?-n:n},Fu=t((function(n,r){for(;;){if(!r.b)return!1;var t=r.b;if(n(r.a))return!0;r=t}})),Ku=O,Bu=t((function(n,r){return j(n&&c(Fu,(function(n){return"0"!==n&&"."!==n}),f(Ku,yr,E,r))?"-":"",r)})),zu=K,Hu=M,Du=function(n){var r=n.a,t=n.b;if("9"===r){var e=Dr(t);return 1===e.$?"01":c(Hu,"0",Du(e.a))}var u,a=zr(r);return a>=48&&57>a?c(Hu,0>(u=a+1)||u>1114111?"�":u>65535?String.fromCharCode(Math.floor((u-=65536)/1024)+55296,u%1024+56320):String.fromCharCode(u),t):"0"},Ju=S,Ru=t((function(n,r){return r.$?Or:Mr(n(r.a))})),Yu=function(n){return c(Hu,n,"")},Qu=e((function(n,r,t){return n>0?f(Qu,n>>1,j(r,r),1&n?j(t,r):t):t})),Xu=t((function(n,r){return f(Qu,n,r,"")})),Pu=e((function(n,r,t){return j(t,c(Xu,n-ht(t),Yu(r)))})),Gu=function(n){for(var r=n.length,t=Array(r),e=0;r>e;){var u=n.charCodeAt(e);55296>u||u>56319?(t[r-e]=n[e],e++):(t[r-e]=n[e+1],t[r-++e]=n[e-1],e++)}return t.join("")},Zu=function(n){var r=c(Wr,".",n);return r.b?p(r.a,r.b.b?r.b.a:"0"):p("0","0")},Uu=t((function(n,r){var t=r.b;return p(n(r.a),t)})),Vu=t((function(n,r){return r.$?n:r.a})),na=e((function(n,r,t){if((e=t)===1/0||e===-1/0||Ju(t))return zu(t);var e,u=0>t,a=Zu(function(n){var r=c(Wr,"e",zu(Wu(n)));if(r.b){if(r.b.b){var t=r.a,e=r.b.a,u=c(Vu,0,function(n){for(var r=0,t=n.charCodeAt(0),e=43==t||45==t?1:0,u=e;n.length>u;++u){var a=n.charCodeAt(u);if(48>a||a>57)return Or;r=10*r+a-48}return u==e?Or:Mr(45==t?-r:r)}(c($t,"+",e)?c(mt,1,e):e)),a=Zu(t),i=j(a.a,a.b),o=0>u?c(Vu,"0",c(Ru,(function(n){return n.a+"."+n.b}),c(Ru,Uu(Yu),Dr(j(c(Xu,Wu(u),"0"),i))))):f(Pu,u+1,"0",i);return j(0>n?"-":"",o)}return j(0>n?"-":"",t=r.a)}return""}(Wu(t))),i=a.a,o=a.b,s=ht(i)+r,v=j(c(Xu,1-s,"0"),f(Pu,s,"0",j(i,o))),b=ht(v),l=c(nt,1,s),d=c(n,u,f(gt,l,b,v)),h=f(gt,0,l,v),g=d?Gu(c(Vu,"1",c(Ru,Du,Dr(Gu(h))))):h,$=ht(g),p="0"===g?g:r>0?0>m(r,ht(o))?f(gt,0,$-r,g)+"."+f(gt,$-r,$,g):j(i+".",f(Pu,r,"0",o)):j(g,c(Xu,Wu(r),"0"));return c(Bu,u,p)}))(t((function(n,r){var t,e=Dr(r);return 1!==e.$&&("5"===e.a.a?""!==e.a.b||!n:(t=zr(e.a.a))>53&&n||t>=53&&!n)}))),ra=Wn("span"),ta=Bn,ea=In,ua=c(lu,qr,c(e((function(n,r,t){return j(c(Xu,n-ht(t),Yu(r)),t)})),2,"0")),aa=function(n){var r=n.g,t=n.i,e=n.ag,u=n.aq,a=100*(60*r+t)/1440;return c(Su,k([Lu("absolute top-0 bottom-0 flex flex-col items-center"),Lu("pointer-events-none"),c(ta,"left","calc("+c(na,2,a)+"% - 27px)")]),k([c(Su,k([Lu("flex-grow w-0 border-2 border-opacity-50"),e]),E),c(ra,k([Lu("p-2 text-sm font-bold rounded-full"),u]),k([ea(ua(r)+":"+ua(t))]))]))},ia=function(n){var r=c(wu,n.A,n.K),t=r.g,e=r.i;return aa({ag:Lu("border-blue-400"),g:t,i:e,aq:Lu("text-blue-100 bg-blue-400")})},ca=Kn,fa=t((function(n,r){return c(ca,n,{$:0,a:r})})),oa=H,sa=c(t((function(n,r){return f(jt,Ot,r,n)})),k(["target","checked"]),oa),va=function(n){var r=n.Q,t=c(wu,n.A,n.W),e=t.g,u=xu(t.i/r)*r,a=60===u?p(e+1,0):p(e,u),i=a.a,f=a.b;return aa({ag:Lu("border-red-400"),g:i,i:f,aq:Lu("text-red-100 bg-red-400")})},ba=function(n){return{$:2,a:n}},la=Lu(c(Ir," ",c(Et,et,c(t((function(n,r){return f(jt,t((function(r,t){return n(r)?c(yr,r,t):t})),E,r)})),(function(n){return n.b}),E)))),da=t((function(n,r){return n?r:la})),ha=t((function(n,r){return r.b?f(jt,yr,r,n):n})),ga=t((function(n,r){return f(jt,ha,E,c(Et,n,r))})),ma=Tu("id"),$a=t((function(n,r){return c(ca,n,{$:3,a:r})})),pa=i((function(n,r,t,e,u,a){return{bn:r,bq:t,bH:n,bK:e,bO:u,bU:a}})),wa=c(st,(function(n){switch(n){case 0:return 1;case 1:return 2;case 2:return 3;case 3:return 4;case 4:return 5;default:return 0}}),c(Ot,"button",Dt)),ya=D,ja=f(vt,t((function(n,r){return p(n,r)})),c(Ot,"clientX",ya),c(Ot,"clientY",ya)),Ea=function(n,r,t,e,u,a,i,c){return 7===n.a?n.f(r,t,e,u,a,i,c):n(r)(t)(e)(u)(a)(i)(c)}(Z,pa,o(G,e((function(n,r,t){return{bk:n,bt:r,bV:t}})),c(Ot,"altKey",oa),c(Ot,"ctrlKey",oa),c(Ot,"shiftKey",oa)),wa,ja,f(vt,t((function(n,r){return p(n,r)})),c(Ot,"offsetX",ya),c(Ot,"offsetY",ya)),f(vt,t((function(n,r){return p(n,r)})),c(Ot,"pageX",ya),c(Ot,"pageY",ya)),f(vt,t((function(n,r){return p(n,r)})),c(Ot,"screenX",ya),c(Ot,"screenY",ya))),Aa=c(e((function(n,r,t){return c($a,n,c(st,(function(n){return{w:t(n),ar:r.ar,av:r.av}}),Ea))})),"mousemove",{ar:!0,av:!1}),xa=t((function(n,r){var t=c(wu,n,r),e=t.g,u=15*xu(t.i/15);return"time-"+(60===u?ua(e+1)+"00":j(ua(e),ua(u)))})),ka=function(n){var r,t,e;return t=(r=function(){switch(n){case 0:return p(Lu("text-green-600 bg-green-200 border-green-600"),"talk");case 1:return p(Lu("text-blue-600 bg-blue-200 border-blue-600"),"movie");default:return p(Lu("text-red-600 bg-red-200 border-red-600"),"talk")}}()).a,e=r.b,c(ra,k([Lu("p-1 border-2 rounded-md"),t]),k([ea(e)]))},Na=Wn("h1"),_a=t((function(n,r){var t=r.M,e=r.I,u=r.E,a=r.J,i=r.N;return c(Su,k([Lu("flex flex-col justify-start border-2 shadow-xl bg-gray-50 rounded-md"),c(ta,"grid-column",c(xa,n,t)+" / "+c(xa,n,e)),c(ta,"grid-row","track-"+qr(i))]),k([c(Na,E,k([ea(u)])),c(Su,k([Lu("self-end")]),k([ka(a)]))]))})),Ca=Tu("type");_e={Main:{init:Mt({bE:function(n){var r=n.al;return p({W:zt(r),Q:1,K:zt(r),ac:E,ae:{af:0,ax:0},A:Ht},Wt(k([c(St,It,Bt),c(St,(function(){return qt}),pt(0))])))},bW:function(){var n;return Rt(k([c(ge,3e4,Jt),(n=t((function(){return qt})),f(Te,1,"resize",c(Ot,"target",f(vt,n,c(Ot,"innerWidth",Dt),c(Ot,"innerHeight",Dt)))))]))},b$:ku,b0:function(n){var r,e,u,a,i,f,o,s,v,b,l;return{bm:k([c(Su,k([Lu("flex flex-col justify-start w-full p-4 overflow-x-hidden space-y-4")]),k([c(Iu,k([Lu("flex items-center")]),k([c(Ou,k([Lu("flex items-center space-x-2")]),k([c(ra,E,k([ea("snap")])),c(Mu,k([Ca("checkbox"),_u(10===n.Q),(l=function(n){return{$:6,a:n?10:1}},c(fa,"change",c(st,l,sa)))]),E)]))])),c(qu,k([Lu("relative")]),k([(r=n,e=r.A,u=r.ac,a=c(Br,1,3),i=t((function(n,r){var t=ua(r);return c(Su,k([Lu("p-1 text-xs bg-blue-200 rounded-lg"),Lu("absolute origin-bottom-left -rotate-45"),c(da,!n,Lu("translate-y-4")),c(ta,"grid-row",n?"top-time":"bottom-time"),c(ta,"grid-column","time-"+t+"00")]),k([ea(t+":00")]))})),f=k([0,15,30,45]),o=c(Br,0,23),s=function(n){return c(Et,i(n),o)},v=c(Et,(function(n){return c(Su,k([Lu("w-0 border-2 border-gray-400"),c(ta,"grid-column","time-"+ua(n)+"00"),c(ta,"grid-row","track-1 / bottom-time")]),E)}),o),b=c(Et,(function(n){var r=n.i;return{g:ua(n.g),i:ua(r)}}),c(ga,(function(n){return c(Et,(function(r){return{g:n,i:r}}),f)}),o)),c(Su,k([Lu("relative my-6 grid gap-y-2"),Aa(c(lu,(function(n){return n.bq}),c(lu,et,ba))),c(ta,"grid-template-rows",c(Ir,"\r",c(yr,"[top-time] 1.5em",j(c(Et,(function(n){return"[track-"+qr(n)+"] 1fr"}),a),k(["[bottom-time] 1.5em"]))))),c(ta,"grid-template-columns",c(Ir,"\r",c(Et,(function(n){return"[time-"+n.g+n.i+"] 1fr"}),b)))]),c(yr,c(Su,k([Lu("bg-gray-100"),c(ta,"grid-column","time-0000 / time"),c(ta,"grid-row","track-1 / bottom-time"),ma("timeline")]),E),j(s(!0),j(v,j(c(Et,_a(e),u),s(!1))))))),ia(n),va(n)]))]))]),E:"Timeline example"}}})(c(ot,(function(n){return{$:0,a:{al:n}}}),c(Ot,"millisecNow",Dt)))(0)}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?d(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,_e):n.Elm=_e}(n),n.Elm.Main.init({flags:{millisecNow:(new Date).getTime()}});
//# sourceMappingURL=index.1eaf07fc.js.map
