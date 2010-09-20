function(doc) {
  if (doc.type && doc.type === "feed" && (outOfDate(doc) || hasErr(doc))) {
      emit(doc._id, doc);
  }
};

function outOfDate(old) {
  return old.updated === 0 || (getTime() - old.updated) > (1 * 60 * 1);
};

function hasErr(doc) {
  return typeof doc.status.cssClass === "error";
};

function getTime() {
  return Math.round(new Date().getTime() / 1000);
};