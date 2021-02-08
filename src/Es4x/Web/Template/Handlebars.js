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
