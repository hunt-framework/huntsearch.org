function completeQuery(query, completions) {
  var result = [];
  if (completions.code === 0)
  {
    query = query.trim();
    var norm = query.replace(/[:\(\)\"]/g," ");
    var rightmost = norm.split (" ").filter(Boolean).pop();

    $(completions.msg).each(function(i, e) {
      var r = query.replace(new RegExp(rightmost 
          + "(?=[^" + rightmost +" ]*$)"), e[0]);
      result.push(r);
    });
  }
  return result
}
