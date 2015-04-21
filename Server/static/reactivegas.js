(function() {
  var app = angular.module('reactivegas', ['ngSanitize']);

  app.controller('Reactivegas', ['$http',function(http){
    var gas = this;
    gas.nomi = [];
    gas.fetch=function(){
      gas.submenu = (gas.select === "manuale.html") 
      http.get(gas.select).success(function(data){
      gas.doc = data;
        })};
    http.get('documenti.json').success(function(elenco){
      gas.navs = elenco;
    });
    http.get('dichiarazioni.json').success(function(elenco){
      gas.nomi = elenco;
    });
  }]);

  app.directive("dichiarazioni", function() {
    return {
      restrict: 'E',
      templateUrl: "dichiarazioni.html",
    };
  });

  app.directive("menunav", function() {
    return {
      restrict: 'E',
      templateUrl: "menunav.html",
    };
  });

})();

