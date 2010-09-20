function(doc) {  
  if (doc.type && doc.type === "feeditem") {
      emit([doc.sourceLink, doc.date], doc);
  }
}