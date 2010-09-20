function(doc) { 
  if (doc.type && doc.type === "batchfeed") {
      emit(null, doc);
  }
}
