$(function() {
  window.parent.postMessage($(document).height().toString(), "*");
  $("pre").addClass("prettyprint");
});
