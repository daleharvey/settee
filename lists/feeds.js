function(head, req) {
  var row;
  start({
    "headers": {
      "Content-Type": "text/html"
    }
  });
  while(row = getRow()) {
    send(row.value);
  }
}