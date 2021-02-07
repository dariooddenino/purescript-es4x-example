var vweb = require('@vertx/web');
var Router = vweb.Router;
var vtempl = require('@vertx/web-templ-handlebars');
var template = vtempl.HandlebarsTemplateEngine.create(vertx);

exports.renderTemplate = function (data) {
    return function (templateName) {
        return function (responseHandler) {
            return function() {
                template.render(JSON.parse(data), templateName, function(res) {
                  responseHandler(res)();
              });
            };
        };
    };
};

exports.getTemplateResult = function (res) {
    return res.result();
};

exports.createRouter = function() {
    return Router.router(vertx);
};

exports.createRoute = function(router) {
    return function(routePath) {
        return function() {
            return router.route(routePath);
        };
    };
};

exports.handleRouter = function(server) {
    return function(router) {
        return function() {
            return server.requestHandler(router);
        };
    };
};

exports.handle = function(route) {
    return function(handler) {
        return function() {
            route.handler(function(ctx) {
                handler(ctx)();
            });
        };
    };
};


exports.createHttpServer = function() {
    return vertx.createHttpServer();
};

exports.handleRequests = function(server) {
    return function(handler) {
        return function() {
            return server.requestHandler(function(ctx) {
                handler(ctx)();
            });
        };
    };
};

exports.request = function(ctx) {
    return function() {
        return ctx.request();
    };
};

exports.getParam = function(req) {
    return function(str) {
        return function() {
            return req.getParam(str);
        };
    };
};

exports.response = function(ctx) {
    return function() {
        return ctx.response();
    };
};

exports.putHeader = function(resp) {
    return function(hName) {
        return function(hVal) {
            return function() {
                return resp.putHeader(hName, hVal);
            }
        }
    }
}

exports.write = function(resp) {
    return function(str) {
        return function() {
            resp.write(str);
        };
    };
};

exports.setChunked = function(resp) {
    return function() {
        resp.setChunked(true);
    };
};

exports.next = function(ctx) {
   return function() {
       ctx.next();
   };
};

exports.end = function(resp) {
    return function() {
        return resp.end();
    };
};

exports.endStr = function(resp) {
    return function(str) {
        return function() {
            return resp.end(str);
        };
    };
};


exports.listen = function(server) {
    return function(port) {
        return function() {
            return server.listen(port);
        };
    };
};
