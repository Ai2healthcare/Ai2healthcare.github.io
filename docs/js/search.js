(function() {

var search = instantsearch({
  appId: 'OMSFKPU7NT',
  apiKey: '9a418bfac894317a5e8cc748086d057a',
  indexName: 'test_cgm',
  urlSync: {},
  searchFunction: function(helper) {
    if (helper.state.query !== '') helper.search();
  }
});

search.addWidget(
  instantsearch.widgets.searchBox({
    container: '#q',
    searchOnEnterKeyPressOnly: true
  })
);

var hitTemplate =
  '<article class="preview">' +
    '<header>' +
      '<h1 class="post-title"><a href="{{url}}">{{{title}}}</a></h1>' +
      '<div class="post-meta">' +
        '<span>{{{autor}}}</span> · ' +
        '<time datetime="{{{date}}}">{{{date}}}</time>' +
      '</div>' +
    '</header>' +
    '<section class="post-excerpt">' +
      '<p>{{{description}}}</p>' +
      '<p class="readmore"><a href="{{url}}">阅读全文 <i class="fa fa-angle-double-right" style="padding-left: 5px;"></i></a></p>' +
    '</section>' +
  '</article>';

var noResultsTemplate =
  '<div class="text-center">No results found matching <strong>{{query}}</strong>.</div>';

search.addWidget(
  instantsearch.widgets.hits({
    container: '#hits',
    hitsPerPage: 10,
    templates: {
      empty: noResultsTemplate,
      item: hitTemplate
    },
    transformData: function(hit) {
      if (!hit.description) {
        hit.description = hit.content.slice(0, 100).replace("\n", "") + '...';
      }
      hit.date = hit.date.slice(0, 10);
      return hit;
    }
  })
);

search.start();

})();
