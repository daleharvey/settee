
var SETTEE = $.sammy(function() {
    this.get("", function () {
        console.log("whoops");
    });
});

SETTEE.run();