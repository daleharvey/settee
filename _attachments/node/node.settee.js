var couch = "http://127.0.0.1:5984";

var http = require('http'), 
    url  = require('url');

function fetchUrl(location, cb) {

  var uri    = url.parse(location), 
      path   = (uri.pathname || "/") + (uri.search || ""),
      client = new http.createClient(uri.port || 80, uri.hostname),
      req    = client.request('GET', path),
      body   = "";
  
  req.addListener('response', function (response) {
    
    response.addListener("data", function (chunk) {
      body += chunk;
    });
    
    response.addListener("end", function() {
      cb(body);
    });
  });  
  
  req.end();
};

function fetchFeeds() {
  
  var feed,
      uri = couch + "/settee/_design/settee/_view/to-update";

  fetchUrl(uri, function(body) {
    var result = JSON.parse(body);
    for (feed in result.rows) {
      fetchUrl(feed, function() {
        console.log("hello?");
      });
    }
  });
  
};

fetchFeeds();
i
// fetchUrl("http://news.ycombinator.com/", function (body) {
//   console.log(body);
// });