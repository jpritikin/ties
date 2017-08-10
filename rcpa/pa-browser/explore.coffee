{ div, h1, h2, h3, h4, p, a, i, select, option, label, form, button,
 thead, tfoot, th, td, tr, table, tbody, input, li, ul, br } = React.DOM

queryString = ((a) ->
  if a is ''
    return {}
  b = {}
  ind = 0
  while ind < a.length
    p = a[ind].split('=', 2)
    if p.length == 1
      b[p[0]] = ''
    else
      b[p[0]] = decodeURIComponent(p[1].replace(/\+/g, ' '))
    ++ind
  b
)(window.location.search.substr(1).split('&'))

descriptionIntro = "This box displays the item that was used to ask
  about the currently hovered characteristic."

descriptionMap =
  skill: "How much skill is required?"
  predict: "How predictable is the action?"
  novelty: "To what extent do you find new and fresh experiences during the activity?"
  creative: "To what extent are there opportunities to express creativity during the activity?"
  complex: "How complex is the action and the environment in which it takes place?"
  goal1: "How clear and straightforward are the short-term rules and goals?"
  feedback1: "How quickly do you know whether what you wanted to accomplish in the short term was successful?"
  goal2: "How clear and straightforward are the long-term rules and goals?"
  feedback2: "How quickly do you know whether what you wanted to accomplish in the long term was successful?"
  chatter: "Does your extraneous mental chatter temporarily shut down during the activity?"
  waiting: "How much time do you feel like is spent waiting for the next step in the activity?"
  body: "How much of your body is involved in the activity? Is it full-body or only part of your body?"
  control: "How much control do you feel like you can exert over your performance?"
  present: "During the activity, to what extent do you feel like you are in the present moment?"
  spont: "How spontaneous is your action?"
  stakes: "During the activity, how important does the activity feel?"
  evaluated: "To what extent do you care about how others judge your performance?"
  reward: "After engaging in the physical activity, to what extent do you feel great?"
  injury1: "What is the risk of minor physical injury (more than just muscle soreness)?"
  injury2: "What is the risk of serious physical injury?"
  flow: "Approximate flow score"

Skills=["Novice", "Amateur", "Expert"]

MAX_ABS_WEIGHT= 3

String.prototype.trunc =
     `function( n, useWordBoundary ){
         if (this.length <= n) { return this; }
         var subString = this.substr(0, n-1);
         return (useWordBoundary 
            ? subString.substr(0, subString.lastIndexOf(' ')) 
            : subString) + "...";
      };`

weightLine = (props) ->
  items = [-MAX_ABS_WEIGHT..MAX_ABS_WEIGHT].map (val) ->
    option
      key: val
      value: val
      val
  tr
    key: props.facet
    td {},
      if props.abbreviateFacet then props.facet else descriptionMap[props.facet]
    td {},
      select
        name: props.facet
        value: props.weight
        onChange: (event) ->
          props.onClick(RCPA_FACETS.indexOf(props.facet), event.target.value)
        items

advancedPanel = (props) ->
  lis = (RCPA_FACETS[x] for x in props.facetOrder).map (text) ->
    weightLine
      facet: text
      abbreviateFacet: props.abbreviateFacet
      onClick: props.onClick
      weight: props.weight[RCPA_FACETS.indexOf text]
  sortIndicatorC = ""
  sortIndicatorW = ""
  sortDir = if props.sortAsc then 'ascending' else 'descending'
  if props.sortedBy is 'characteristic' then sortIndicatorC = sortDir
  if props.sortedBy is 'weight'         then sortIndicatorW = sortDir
  table
    className: "ui very compact table sortable"
    thead {},
      tr {},
        th
          className: "sorted #{sortIndicatorC}"
          onClick: ->
            if sortIndicatorC is "descending"
              props.setFacetOrder null, null, [0..RCPA_FACETS.length-1]
              return
            props.sortAsc = true if sortIndicatorC is ""
            props.sortAsc = false if sortIndicatorC is "ascending"
            newOrder = props.facetOrder.slice()
            newOrder.sort (a,b) ->
              [a,b]=[b,a] if not props.sortAsc
              RCPA_FACET_ALPHA[a] - RCPA_FACET_ALPHA[b]
            props.setFacetOrder "characteristic", props.sortAsc, newOrder
          "Characteristic"
        th
          className: "sorted #{sortIndicatorW}"
          onClick: ->
            if sortIndicatorW is "descending"
              props.setFacetOrder null, null, [0..RCPA_FACETS.length-1]
              return
            props.sortAsc = true if sortIndicatorW is ""
            props.sortAsc = false if sortIndicatorW is "ascending"
            newOrder = props.facetOrder.slice()
            newOrder.sort (a,b) ->
              [c,d]=[a,b]
              [c,d]=[b,a] if not props.sortAsc
              cmp = props.weight[c] - props.weight[d]
              cmp = a - b if cmp is 0
              cmp
            props.setFacetOrder "weight", props.sortAsc, newOrder
          "Weight"
    tbody {},
      lis
    tfoot {},
      tr {},
        th {},
          div
            className: "ui toggle checkbox"
            input
              className: "hidden"
              type: "checkbox"
              checked: props.abbreviateFacet
            label
              onClick: ->
                props.setAbbreviateFacet(!props.abbreviateFacet)
              "Abbreviate"
        th {},
              button
                className: "negative ui button"
                onClick: ->
                  $("#reset-weights-modal").modal
                    onApprove: ->
                      props.resetWeights()
                  .modal("show")
                "Reset"

countNonzero = (vec) ->
  nonzero = 0
  nonzero += 1 for w in vec when w isnt 0
  nonzero

weightPanel = (props) ->
  panel = if props.advancedSort
    advancedPanel(props)
  else
    curWeight = props.weight.findIndex (w) -> w != 0
    div {},
      br(),
      select
        value: RCPA_FACETS[curWeight]
        onChange: (event) ->
          index = RCPA_FACETS.indexOf event.target.value
          props.setSingleWeight(index)
        (RCPA_FACETS[x] for x in props.facetOrder).map (text) ->
          option
            value: text
            descriptionMap[text].trunc(80,1)
      br(),
      br()
  div
    className: "column"
    div
      className: "ui message"
      div
        className: "header"
        "Which characteristic#{if props.advancedSort then 's' else ''} do you value?"
      panel,
      div
        className: "ui toggle checkbox"
        input
          className: "hidden"
          type: "checkbox"
          checked: props.advancedSort
        label
          onClick: ->
            nonzero = 0
            nonzero += 1 for w in props.weight when w isnt 0
            if props.advancedSort and nonzero > 1
              $("#reset-weights-modal").modal
                    onApprove: ->
                      props.setAdvancedSort(!props.advancedSort)
              .modal("show")
            else
              props.setAdvancedSort(!props.advancedSort)
          "Show advanced controls"

dotprod = (v1, v2) ->
  throw new Error("dotprod: #{v1.length} != #{v2.length}") if v1.length isnt v2.length
  maxIndex = v1.length - 1
  out = 0
  out += v1[x] * v2[x] for x in [0..maxIndex] when (v1[x] isnt "NA" and v2[x] isnt "NA")
  out

prettyName = (name) ->
  part = name.split(';')
  return name if part.length is 1
  return "#{part[0]} (#{part[1]})" if part.length is 2
  throw new Error("How to prettyName #{name}?")

resultPanel = (props) ->
  maxPA = RCPA_PA.length - 1
  score = new Array(RCPA_PA.length)
  score[x] = dotprod(props.weight, RCPA_DATA[x]) for x in [0..maxPA]
  order = [0..maxPA]
  order.sort (a,b) ->
    cmp = score[a] - score[b]
    if cmp is 0
      cmp = -1 if RCPA_PA[a] < RCPA_PA[b]
      cmp = 1  if RCPA_PA[a] > RCPA_PA[b]
    cmp
  # when list gets too long, only show beginning and end TODO
  lis = order.map (index) ->
    rawName = RCPA_PA[index]
    if RCPA_PA_SAMPLESIZE[index] < 10
      color = "#999"
    else
      color = "#000"
    part = rawName.split(';')
    div
      className: "item"
      key: index
      style:
        color: color
      "#{prettyName(RCPA_PA[index])} "
      a
        href: "https://en.wikipedia.org/wiki/#{encodeURI(part[0])}"
        target: "_blank"
        "^"
  div
    className: "column"
    # div
    #   className: "ui buttons"
    #   Skills.map (text) ->
    #     isActive = ""
    #     isActive = "active" if text is props.skill
    #     div
    #       className: "ui button #{isActive}"
    #       key: text
    #       onClick: () ->
    #         props.setSkill(text)
    #       text
    div
      className: "ui list"
      lis

class Graph extends React.Component
  constructor: (props) ->
    super(props)
    @state =
      network: null

  componentDidMount: ->
    data =
      nodes: RCPA_nodes
      edges: RCPA_edges
    options =
      height: '700px'
      nodes:
        shape: 'dot'
      interaction:
        navigationButtons: true
        keyboard:
          enabled: true
          bindToWindow: false
    elem = $('#graph')
    @state.network = new vis.Network(elem[0], data, options)

  componentWillUnmount: ->
    if network? then network.destroy()

  render: ->
    div
      className: "ui message"
      div
        id: "graph"
      div
        className: "ui info message"
        "Scroll or use the square bracket keys '[' and ']' to zoom in and out.
        Drag background or use arrow keys to recenter.
        Thickness of the line indicate how many respondents chose the pair.
        Green, brown, and dark red correspond to novice, amateur, and expert, respectively."

class Jumbotron extends React.Component
  constructor: (props) ->
    super(props)
    iw = RCPA_FACETS.map (facet) ->
      val = Math.round(Number(queryString[facet]))
      aval = Math.abs(val)
      if aval <= MAX_ABS_WEIGHT then val
      else 0
    advancedSort = countNonzero(iw) > 1
    if not advancedSort and countNonzero(iw) == 0
      iw[RCPA_FACETS.length-1] = 1.0
    @state =
      abbreviateFacet: false
      advancedSort: advancedSort
      weight: iw
      skill: Skills[1]
      facetOrder: [0..RCPA_FACETS.length-1]
      sortedBy: null
      sortAsc: null

  maybeResetSort: ->
    return if @state.sortedBy isnt "weight"
    this.setState
      sortedBy: null
      sortAsc: null

  setFacetOrder: (col, asc, order) =>
    this.setState
      sortedBy: col
      sortAsc: asc
      facetOrder: order

  handleChange: (index, value) =>
    this.maybeResetSort()
    foo = @state.weight.slice()
    foo[index] = Number(value)
    this.setState
      weight: foo

  resetWeights: =>
    this.maybeResetSort()
    foo = @state.weight.slice()
    foo.fill(0)
    this.setState
      weight: foo

  setSingleWeight: (index) =>
    this.maybeResetSort()
    foo = @state.weight.slice()
    foo.fill(0)
    foo[index] = 1
    this.setState
      weight: foo

  setAbbreviateFacet: (current) =>
    this.setState
      abbreviateFacet: current

  setAdvancedSort: (current) =>
    if not current
      foo = @state.weight.slice()
      if countNonzero(foo) != 1
        foo.fill(0)
        foo[RCPA_FACETS.length-1] = 1
      console.log(foo)
      this.setState
        advancedSort: current
        weight: foo
    else
      this.setState
        advancedSort: current

  setCurrentSkill: (value) =>
    this.setState
      skill: value

  componentDidMount: ->
    $('#content').visibility
      onUpdate: (calc) ->
        if calc.width < 960 then $('#table-of-contents').hide()
        else $('#table-of-contents').show()

  render: ->
    parent = this
    qs1 = (x) -> [RCPA_FACETS[x], parent.state.weight[x]].join('=')
    weightQS = (qs1 x for x in [0..RCPA_FACETS.length-1] when @state.weight[x] isnt 0)
    shareURL = [window.location.href.split('?')[0], weightQS.join('&')].join('?')
    responses = RCPA_edges.map (node) ->
      val = node.value
      if val then val
      else 0
    div {},
      div
        className: "ui right rail screen-only"
        id: "table-of-contents"
        style:
          position: "fixed"  # visibility is too confusing, force it
          left: "80%"
          marginTop: "30px"
        div
          className: "ui segment"
          h4
            className: "ui header"
            "Contents"
          div
            className: "ui vertical text menu"
            div
              className: "content menu" # Should auto-generate this with jquery TODO
              a
                className: "item"
                href: "#introduction"
                "Introduction"
              a
                className: "item"
                href: "#data-explorer"
                "Data Explorer"
              a
                className: "item"
                href: "#data-connectivity"
                "Data Connectivity"
              a
                className: "item"
                href: "#invitation"
                "Invitation to Participate"
              a
                className: "item"
                href: "#contact"
                "Questions?"
      h1
        className: "ui header center aligned"
        "Preliminary results"
      h2
        className: "ui header center aligned"
        "Relative characteristics of physical activities"
      h3
        className: "ui header center aligned"
        "updated #{RCPA_date}"
      div
        className: "ui text container"
        h3
          className: "ui header"
          id: "introduction"
          "Introduction"
        "You just completed a "
        a
          href: "http://tiny.cc/physical"
          "survey"
        ". Not all of the data is in, but we wanted to show you what can be gleaned
        from preliminary data. We also include a status update on the data collection effort.
        You may prefer to start with a video introduction."
        br(),
        br(),
        div
          dangerouslySetInnerHTML:
            __html: '<iframe width="560" height="315" src="https://www.youtube.com/embed/qANOOKesBkQ?rel=0" frameborder="0" allowfullscreen></iframe>'
        h3
          className: "ui header"
          id: "data-explorer"
          "Data Explorer"
        "Below is a drop-down menu of the characteristics that you
        considered for your pair of physical activities.
        When you choose one of the characteristics,
        the order of the list of activities below will update.
        For example, if you choose 'How much of your body is involved...'
        then one of the first activities will be hatha yoga and one of
        last activities will be walking."
        br(),
        br(),
        "Advanced controls: On the left you'll see a list of the characteristics that you
        considered for your pair of physical activities.
        Choose one of the characteristics that you value, or are curious about,
        and assign it a +1 or -1 weight.
        When you assign a weight, the order of the list of activities on the right side
        will update to reflect the preference that you have expressed, from low to high
        (or high to low for negative weights).
        You can play around with assigning more than one weight at a time and see
        how the order changes.
        Are there physical activities that you haven’t tried that might be worth considering?"
        br(),
        br(),
        "Currently, sample size is limited. Activities with fewer than 10 comparisons are
        shown in gray. Rankings may not match intuition very well.
        Simulations suggest that 50-100 comparisons are required for stable estimates."
        br()
        br()
        div
          className: if @state.advancedSort then "ui stackable two column grid" else ""
          weightPanel
            onClick: parent.handleChange
            abbreviateFacet: @state.abbreviateFacet
            setAbbreviateFacet: parent.setAbbreviateFacet
            advancedSort: @state.advancedSort
            setAdvancedSort: parent.setAdvancedSort
            weight: @state.weight
            resetWeights: parent.resetWeights
            setSingleWeight: parent.setSingleWeight
            facetOrder: @state.facetOrder
            sortedBy: @state.sortedBy
            sortAsc: @state.sortAsc
            setFacetOrder: parent.setFacetOrder
          resultPanel
            weight: @state.weight
            skill: @state.skill
            setSkill: parent.setCurrentSkill
        br()
        div
          className: "ui form"
          div
            className: "field"
            label
              "Share your weights (copy & paste the link)"
            input
              type: "text"
              readOnly: true
              value: shareURL
              onMouseEnter: (ev) ->
                ev.target.select()
              onClick: (ev) ->
                ev.target.select()
              onFocus: (ev) ->
                ev.target.select()
        h3
          className: "ui header"
          id: "data-connectivity"
          "Data Connectivity"
        "The graph below shows which pairs of activities were compared by participants.
        Although #{RCPA_nodes.length} activities have been mentioned,
        we can only analyze those that are not home to isolated islands.
        World Sports Encyclopedia (2003) estimated that there are about eight thousand sports.
        Of course, physical activities are a superset of that.
        Even if we restrict our interest to, say, the 400 most popular physical activities,
        there are still about eighty thousand possible pairs.
        So far, data is available for #{RCPA_edges.length} pairs
        from #{responses.reduce((x,y)->x+y)} responses.
        Our statistical model can fill in the gaps, but the more data the better."
        React.createElement(Graph)
        h3
          className: "ui header"
          id: "invitation"
          "Invitation to Participate"
        "If you'd like to contribute more data,
        you can take this survey again with a different pair of physical activities.
        Or you may want to invite others to participate.
        The link to the survey is "
        a
          href: "http://tiny.cc/physical"
          "http://tiny.cc/physical"
        h3
          className: "ui header"
          id: "contact"
          "Questions?"
        "Joshua Pritikin, Ph.D. <"
        a
          href: "mailto:jpritikin@pobox.com"
          "jpritikin@pobox.com"
        ">"
        br()
        "Virginia Institute for Psychiatric and Behavioral Genetics"
        br()
        "Virginia Commonwealth University"
        br()
        "800 E Leigh St, Biotech One"
        br()
        "Suite 1-133"
        br()
        "Richmond, VA 23219"
        br()
        br()
        br()
        br()
        br()
        br()
        br()
        br()
        br()
        br()
        br()
        br()
        br()
        br()

ReactDOM.render(React.createElement(Jumbotron), document.getElementById('content'))
