function(doc) {
    
  var exports = {}; // So md5.js doesnt blow up
  // !code vendor/couchapp/lib/md5.js 
    
  if (doc.type && doc.type === "batchfeed") {
    
    var i, item, body, date,
        re   = /^<\?xml\s+version\s*=\s*(["'])[^\1]+\1[^?]*\?>/,
        str  = doc.body.replace(re, ""),
        feed = new XML(str);

    // this is nasty, but its rss, its supposed to be nasty
    // duck type rss vs atom
    if (feed.channel.length() > 0) {
      
      for (i = 0; i < feed.channel.item.length(); i++) {
        
        item = feed.channel.item[i];
        body = item.description.toString();
        date = new Date(item.pubDate.toString()).getTime();
        
        if (!date) { 
         date = new Date().getTime();
        }	
        
        emit(date + hex_md5(body), { 
          title : item.title.toString(),
          body  : body,
          link  : item.link.toString(),
          date : Math.round(date / 1000),
          sourceId : doc._id,
          sourceTitle : feed.channel.title.toString(),
          sourceLink : doc.source
        });
      }
    } else {

      default xml namespace="http://www.w3.org/2005/Atom";

      for each (item in feed..entry) { 

        body = item.content.toString();
        date = new Date(item.updated.toString()).getTime();
        
        if (!date) { 
         date = new Date().getTime();
        }
          
        emit(date + hex_md5(body), {
          title : item.title.toString(),
          body  : body,
          link  : item.link[0].@href.toString(),
          date : Math.round(date / 1000),
          sourceId : doc._id,
          sourceTitle : feed.title.toString(),
          sourceLink : doc.source
        });
      }
    }
  }
}