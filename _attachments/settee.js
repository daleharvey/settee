var SETTEE = (function() {
  
  var dbName        = "settee",
      router        = new Router(),
      db            = $.couch.db(dbName),
      perPage       = 21,
      defaultOpts   = {descending: true, limit: perPage},
      subscriptions = {},
      views         = {};
  
  router.get("", function() {
    showFeedItems(defaultOpts);
  });

  router.get("items/:key", function(key) {
    showFeedItems(createOpts(key));
  });

  router.get("item/:page/:item", function(page, item) {
    showFeedItem("feed-items", "key", createOpts(page), item);
    //showFeedItem("all-feeds", "key", createOpts(page), item);
  });

  router.get(/sub\/([^\/]+)\/(.+)/, function(item, id) {
    ensureCache("all-feeds", false, function (feeddata) {        

      // eugh, I strip http:// because it screws with regex matches
      // for the router
      var obj = keySearch(feeddata.rows, "id", id);
      if (!obj) {
        id = "http://" + id;
        obj = keySearch(feeddata.rows, "id", id);
      }

      var opts = {
          endkey     : JSON.stringify([id]),
          startkey   : JSON.stringify([id, {}]),
          descending : true
      };

      showFeedItem("items-by-feed", "id", opts, item);
      //showFeedItem2(opts, item);
    });
  });
  
  router.get(/subscription\/(.+)/, function(id) {
    
    ensureCache("all-feeds", false, function (feeddata) {        

      // eugh, I strip http:// because it screws with regex matches
      // for the router
      var obj = keySearch(feeddata.rows, "id", id);
      if (!obj) {
        id = "http://" + id;
        obj = keySearch(feeddata.rows, "id", id);
      }

      var opts = {
          endkey     : JSON.stringify([id]),
          startkey   : JSON.stringify([id, {}]),
          descending : true
      };
      
      ensureCache("items-by-feed", opts, function (data) {
        
        var i, rows = [],
            oldRows = data.rows.slice(0),
            tmp = cloneObj(obj.value);
        
        for (i = 0; i < oldRows.length; i += 1) {
          rows.push(cloneObj(oldRows[i].value));
          rows[i].date = new Date(toJsTime(oldRows[i].value.date))
            .format("mmm/dd/yy HH:MM");
          rows[i].href = "#sub/" + oldRows[i].value._id
            + "/" + oldRows[i].value.sourceLink;
          rows[i].unread = (oldRows[i].value.read === true)
            ? "read" : "unread";
        }
        tmp.rows = rows;
        
        $("#mainpanel")
          .html(Mustache.to_html($("#subscription_tpl").html(), tmp));
      });
    });
  });

  router.post("delete", function(e) {
    
    ensureCache("all-feeds", false, function(data) {
      
      var form = $(e.target),
          id   = form.find("input[name=id]").val(),
          doc  = keySearch(data.rows, "key", id).value;
      
      db.removeDoc({_id: doc._id, _rev: doc._rev});
      document.location.href = "#";
    });
  });
  
  router.post("addfeed", function(e) {
    
    var doc = {
      _id     : $("#subscribeurl").val(),
      url     : $("#subscribeurl").val(),
      type    : "feed",
      status  : {text : "not read yet", cssClass:"ok"},
      added   : toUnixTime(new Date().getTime()),
      updated : 0
    };
    
    $.couch.db(dbName).saveDoc(doc, {
      success : function() { showFeedback($("#feedback"), "saved"); },
      error   : function() { showFeedback($("#feedback"), "already exists"); }
    });
    
  });

  // Create the parameters sent to the view listing determing from
  // the key given
  function createOpts(key) {
    
    var opts = cloneObj(defaultOpts);

    // ! means we are paging backwards, so reverse view
    if (key[0] === "!") {
      opts.descending = false;
      key = key.slice(1);
    }

    opts.startkey = "\""+key+"\"";
    return opts;
  };
  
  function showFeedItem(feed, key, opts, item) {
    
    ensureCache(feed, opts, function (data) {
      
      var row     = keySearch(data.rows, key, item),
          rowCopy = cloneObj(row.value);
      
      // Dunno where json is losing bools, TODO, there is also a race
      // condition with changes
      if (row.value.read+'' !== "true") {
        row.value.read = true;
        db.saveDoc(row.value, {
          "error": function() { console.log("on noes"); }
        });
      }

      // fun way to remove script tags
      var tmp = $("<foo>" + rowCopy.body + "</foo>");
      tmp.find("script").remove();
      rowCopy.body = tmp.html();
      
      rowCopy.current = (feed === "items-by-feed")
        ? "#subscription/" + rowCopy.sourceLink
        : pagerFromOpts(opts);
      
      rowCopy.date    = new Date(toJsTime(rowCopy.date))
        .format("mmm/dd/yy HH:MM");
      
      $("#mainpanel").html(Mustache.to_html($("#item_tpl").html(), rowCopy));
      
    });    
  };
    
  function showFeedItem2(opts, item) {
      
    ensureCache("items-by-feed", opts, function (data) {

      var row     = keySearch(data.rows, "id", item),
          rowCopy = cloneObj(row.value);
      
      // Dunno where json is losing bools, TODO, there is also a race
      // condition with changes
      if (row.value.read+'' !== "true") {
        row.value.read = true;
        db.saveDoc(row.value, {
          "error": function() { console.log("on noes"); }
        });
      }

      // fun way to remove script tags
      var tmp = $("<foo>" + rowCopy.body + "</foo>");
      tmp.find("script").remove();
      rowCopy.body = tmp.html();
      
      rowCopy.current = "#subscription/" + rowCopy.sourceLink;
      rowCopy.date    = new Date(toJsTime(rowCopy.date))
        .format("mmm/dd/yy HH:MM");
      
      $("#mainpanel").html(Mustache.to_html($("#item_tpl").html(), rowCopy));
      
    });    
  };  
    
  function toUnixTime(time) {
    return Math.round(time / 1000);
  };
  function toJsTime(time) {
    return time * 1000;
  };

  function pagerFromOpts(opts) {
    if (opts.startkey) {
      return ((opts.descending) ? "" : "!") + opts.startkey.replace(/\"/g, "");
    }
    return false;
  };
  
  function showFeedItems(opts) {

    var tpl = $("#feeds_tpl"),
        dom = $("#mainpanel");
    
    showView("feed-items", opts, dom, tpl, function(data) {
      
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
      var current = pagerFromOpts(opts) || rows[0] && rows[0].key || "";
      
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
          href     : "#item/" + current + "/" + item.key,
          date     : new Date(toJsTime(v.date)).format("mmm/dd/yy HH:MM"),
          srcTitle : v.sourceTitle,
          title    : v.title
        });
      }
      return vars;      
    });
  };  
  
  function ensureCache(view, opts, cb) {

    console.log("err", opts);
    
    var id  = view + (opts && JSON.stringify(opts) || ""),
        url = "/" + dbName + "/_design/" + dbName + "/_view/" + view;
    
    if (typeof views[id] === "undefined") {
      $.get(url, opts, function (data) {
        views[id] = data;
        cb(views[id]);
      }, "json");      
    } else {
      cb(views[id]);
    }
  };
  
  function showView(view, opts, $id, $template, cb) {
    ensureCache(view, opts, function (data) { 
      $id.html(Mustache.to_html($template.html(), cb(data)));
    });
  };
  
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
      "include_docs=true&feed=longpoll&since=" + seq;
    $.get(url, function(data) {
      views = {};
      fetchSubscriptions();
      router.refresh();
      badComet(data.last_seq);
    }, "json");
  };

  db.info({
    "success": function(data) {
      badComet(data.update_seq);
    }
  });
  
})();

