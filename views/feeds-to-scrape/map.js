function(doc) {
  if (doc.type && doc.type === "feed") {
      emit(doc.updated, doc);
  }
};