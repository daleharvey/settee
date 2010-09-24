function(doc) {  
  if (doc.type && doc.type === "feeditem") {
    emit(doc.date + doc._id, {
      "_id":  doc._id,  
      "_rev": doc._rev,
      "read": doc.read, 
      "title": doc.title, 
      "date":  doc.date,
      "sourceTitle": doc.sourceTitle
    });
  }
}