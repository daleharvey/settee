function(doc) {  
  if (doc.type && doc.type === "feeditem") {
    emit(doc.date + doc._id, doc);
  }
}