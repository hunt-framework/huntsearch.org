
var Application;
(function(Application) {
    (function(Controller) {
        var HuntController = (function() {
            function HuntController($scope, $http, $location, $sce) {
                this.$scope = $scope;
                this.$http = $http;
                this.$location = $location;
                this.$sce = $sce;
            }
            HuntController.$inject = ['$scope', '$http', '$location', '$sce'];
            return HuntController;
        })();
        Controller.HuntController = HuntController;
    })(Application.Controller || (Application.Controller = {}));
    var Controller = Application.Controller;
})(Application || (Application = {}));

var app = angular.module('Application', [
    'ngRoute'
]);

app.config(['$routeProvider', function($routeProvider) {
        $routeProvider.
                when('/home', {
                    templateUrl: './home.html'
                }).
                when('/blog', {
                    templateUrl: './blog.html'
                }).
                when('/documentation', {
                    templateUrl: './documentation.html',
                }).
                when('/about', {
                    templateUrl: './about.html',
                }).
                otherwise({
                    redirectTo: '/home'
                });
    }]);

app.controller('HuntController', Application.Controller.HuntController);

