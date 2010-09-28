var SETTEE = (function() {
  
  var dbName        = "settee",
      router        = new Router(),
      db            = $.couch.db(dbName),
      perPage       = 21,
      defaultOpts   = {descending: true, limit: perPage},
      readItems     = {},
      views         = {};
  
  router.get("", function() {
    showFeedItems(defaultOpts);
  });
  
  router.get("items/:key", function(key) {
    showFeedItems(createPagerOpts(key));
  });
  
  router.get("item/:page/:item", function(page, id) {
    var back = (page === "home") ? "#" : "#items/" + page;
    showFeedItem(back, id);
  });

  router.get(/sub\/([^\/]+)\/(.+)/, function(id, feed) {
    showFeedItem("#subscription/" + feed, id);    
  });
  
  router.get(/subscription\/(.+)/, showSubscription);
  
  router.post("delete", function(e) {
    
    ensureViewCache("all-feeds", false, function(data) {
      
      var form = $(e.target),
          id   = form.find("input[name=id]").val(),
          doc  = keySearch(data.rows, "key", id).value;
      
      db.removeDoc({_id: doc._id, _rev: doc._rev});
      document.location.href = "#";
    });
  });
  
  router.post("addfeed", function(e) {
    
    var url = $("#subscribeurl").val(),
        doc = {
          _id     : url,
          url     : url,
          type    : "feed",
          status  : {text : "not read yet", cssClass:"ok"},
          added   : toUnixTime(new Date().getTime()),
          updated : 0
        };
    
    $.couch.db(dbName).saveDoc(doc, {
      success : function() {
        document.location.href = "#subscription/" + url; 
        showFeedback($("#feedback"), "saved");
        $("#subscribeurl").val("");
      },
      error : function() {
        showFeedback($("#feedback"), "already exists");
      }
    });
    
  });

  function showSubscription(id) {
    
    ensureViewCache("all-feeds", false, function (feed) {
      
      // eugh, I strip http:// because it screws with regex matches
      // for the router, need to search for invalid urls that dont start
      // width http:// too
      var opts = createSubOpts(id, feed.rows),
          doc = keySearch(feed.rows, "id", id);
      
      if (!doc) {
        id = "http://" + id;
        doc = keySearch(feed.rows, "id", id).value;
      } else {
        doc = doc.value;
      }

      ensureViewCache("items-by-feed", opts, function (data) {

        var i, item, rows = [];
        
        for (i = 0; i < data.rows.length; i += 1) {
          
          item = data.rows[i].value;
          rows.push({
            title       : item.title,
            sourceTitle : item.sourceTitle,
            date        : formatDate(toJsTime(item.date)),
            href        : "#sub/" + item._id + "/" + item.sourceLink,
            unread      : (item.read) ? "read" : "unread"
          });
          
        }
        
        if (doc.status && doc.status.cssClass === "error") {
          doc.error = "<div class='error'><strong>error!</strong> "
            + doc.status.text + "</div>";
        }

        doc.rows = rows;
        doc.updated = doc.updated
          ? prettyDate(new Date(toJsTime(doc.updated)))
          : "Never";
        
        render("#mainpanel", "#subscription_tpl", doc);
        
      });
    });
  };
  
  // Create the parameters sent to the view listing determing from
  // the key given
  function createPagerOpts(key) {
    
    var opts = cloneObj(defaultOpts);

    // ! means we are paging backwards, so reverse view
    if (key[0] === "!") {
      opts.descending = false;
      key = key.slice(1);
    }

    opts.startkey = "\""+key+"\"";
    return opts;
  };
  
  function createSubOpts(id, rows) {
    
    // eugh, I strip http:// because it screws with regex matches
    // for the router
    var obj = keySearch(rows, "id", id);
    if (!obj) {
      id = "http://" + id;
      obj = keySearch(rows, "id", id);
    }
    
    return {
      endkey     : JSON.stringify([id]),
      startkey   : JSON.stringify([id, {}]),
      descending : true
    };
  };
  
  function showFeedItem(backLink, id) {

    var show = function(doc) {

      if (!doc.read) {
        doc.read = true;
        // race condition when marking read
        db.saveDoc(doc, { "error": function(){} });
      }      
      
      doc.body = stripScriptTags(doc.body);
      doc.backlink = backLink;    
      doc.date = formatDate(toJsTime(doc.date));

      render("#mainpanel", "#item_tpl", doc);
    };

    if (typeof readItems[id] === "undefined") {
      db.openDoc(id, {
        success : function(data) {
          readItems[id] = data;
          show(cloneObj(data));
        }
      });
    } else {
      show(cloneObj(readItems[id]));
    };
  }

  function stripScriptTags(html) {
    var tmp = $("<foo>" + html + "</foo>");
    tmp.find("script").remove();
    return tmp.html();
  };
  
  function render(dom, tpl, data) {
    $(dom).html(Mustache.to_html($(tpl).html(), data));
  };
  
  function toUnixTime(time) {
    return Math.round(time / 1000);
  };
  function toJsTime(time) {
    return time * 1000;
  };

  function pagerFromOpts(opts) {
    if (opts.startkey) {
      return ((opts.descending) ? "" : "!")
        + opts.startkey.replace(/\"/g, "");
    }
    return false;
  };
  
  function showFeedItems(opts) {

    var tpl = $("#feeds_tpl"),
        dom = $("#mainpanel");
    
    showView("feed-listing", opts, dom, tpl, function(data) {
      
      var offset = data.offset,
          rows   = data.rows.slice(0),
          total  = data.total_rows,
          start  = (opts.descending ? offset : total - (offset + perPage)),
          end    = start + perPage;
      
      if (end > total) {
        end = total;
      }

      // link to current feeds page, is needed for cache lookup if user
      // reads individual item
      var current = pagerFromOpts(opts) || "home";
      
      if (!opts.descending) {
        rows.reverse();
      }

      var newer = (start > 0)
        ? "<a href='#items/!" + rows[0].key + "'>&lsaquo; newer</a>" : "";
      var older = (end < total) ? "<a href='#items/" +
        rows[rows.length-1].key + "'>older &rsaquo;</a>" : "";
      
      var i, item, status,
          vars = {
            newer   : newer,
            older   : older,
            total   : total,
            start   : start + 1,
            end     : end,
            rows    : []
          };

      for (i = 0; i < rows.length; i += 1) {
        item = rows[i];
        v = item.value;        
        vars.rows.push({
          unread   : (v.read === true) ? "read" : "unread",
          href     : "#item/" + current + "/" + v._id,
          date     : formatDate(toJsTime(v.date)),
          srcTitle : v.sourceTitle,
          title    : v.title
        });
      }
      return vars;      
    });
  };
  
  function ensureViewCache(view, opts, cb) {
    
    var id  = view + (opts && JSON.stringify(opts) || ""),
        url = "/" + dbName + "/_design/" + dbName + "/_view/" + view;
    
    if (typeof views[id] === "undefined") {
      $.get(url, opts, function (data) {
        views[id] = data;
        cb(cloneObj(views[id]));
      }, "json");      
    } else {
      cb(cloneObj(views[id]));
    }
  };
  
  function showView(view, opts, $id, $template, cb) {
    ensureViewCache(view, opts, function (data) {
      render($id, $template, cb(data));
    });
  };

  function formatDate(date) {
    
    var now   = new Date(),
        today = new Date(now.getFullYear(), now.getMonth(), now.getDate());

    if (date > today.getTime()) {
      return new Date(date).format("HH:MM");
    } else if (date > new Date(now.getFullYear(), 0, 0).getTime()) {
      return new Date(date).format("dd mmm");
    }
    return new Date(date).format("dd/mm/yyyy");
  }
    
  function keySearch(arr, key, val) {
    for (var i = 0; i < arr.length; i += 1) {
      if (arr[i][key] === val) {
        return arr[i];
      }
    }
    return false;
  };

  function showFeedback($el, msg) { 
    $el.text(msg);
    setTimeout(function () {
      $el.fadeOut();
    }, 3000);    
  };

  function cloneObj(obj) {
    return jQuery.extend(true, {}, obj);
  };
  
  function bindEvents() { 
    router.init();
  };

  function fetchSubscriptions() {
    
    var tpl = $("#subscriptions_tpl");
    showView("all-feeds", false, $("#subscriptions"), tpl, function(data) {
      
      var i, item, vars = {rows:[]};
      
      for (i = 0; i < data.rows.length; i += 1) {
        item = data.rows[i];
        vars.rows.push({
          desc : item.value.title || item.value._id.replace("http://", ""),
          url  : item.value._id.replace("http://", "")
        });
      }
      return vars;
    });    
  };
  
  bindEvents();
  fetchSubscriptions();
  
  // this must be a nasty way of invalidating views, cant really
  // see if incremental view updates are possible
  function badComet(seq) {
    var url = "/" + dbName + "/_changes?heartbeat=10000&" +
      "&feed=longpoll&since=" + seq;
    $.ajax({
      url      : url,
      method   : "GET",
      dataType : "json",
      error    : function() { console.error(arguments); },
      success  : function(data) {
        if (data) { 
          views = {};
          fetchSubscriptions();
          router.refresh();
          badComet(data.last_seq);
        }
      }
    });
  };

  db.info({
    "success": function(data) {
      setTimeout(function() { 
        badComet(data.update_seq);
      }, 100);
    }
  });
  
})();

