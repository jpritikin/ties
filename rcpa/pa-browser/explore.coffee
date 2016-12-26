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

Skills=["Novice", "Amateur", "Expert"]

RCPA_DATA=
  Novice: RCPA_DATA1
  Amateur: RCPA_DATA2
  Expert: RCPA_DATA3

weightLine = (props) ->
  items = [-5..5].map (val) ->
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

weightPanel = (props) ->
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
  div
    className: "column"
    div
      className: "ui message"
      div
        className: "header"
        "Which characteristics do you value?"
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
  score[x] = dotprod(props.weight, RCPA_DATA[props.skill][x]) for x in [0..maxPA]
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
    part = rawName.split(';')
    div
      className: "item"
      key: index
      a
        href: "https://en.wikipedia.org/wiki/#{encodeURI(part[0])}"
        target: "_blank"
        prettyName(RCPA_PA[index])
  div
    className: "column"
    div
      className: "ui buttons"
      Skills.map (text) ->
        isActive = ""
        isActive = "active" if text is props.skill
        div
          className: "ui button #{isActive}"
          key: text
          onClick: () ->
            props.setSkill(text)
          text
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
        "Scroll or use keys '[' and ']' to zoom in and out.
        Drag background or use arrow keys to recenter.
        Size of node and thickness of edge indicate sample size.
        Activities with only one sample are excluded from the model."

class Jumbotron extends React.Component
  constructor: (props) ->
    super(props)
    iw = RCPA_FACETS.map (facet) ->
      val = Math.round(Number(queryString[facet]))
      aval = Math.abs(val)
      if aval <= 5 then val
      else 0
    @state =
      abbreviateFacet: false
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

  setAbbreviateFacet: (current) =>
    this.setState
      abbreviateFacet: current

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
    div {},
      div
        className: "ui right rail"
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
        "Relative characteristics of physical activities:"
      h2
        className: "ui header center aligned"
        "Preliminary results"
      h3
        className: "ui header center aligned"
        "updated #{RCPA_date}"
      div
        className: "ui text container"
        h3
          className: "ui header"
          id: "introduction"
          "Introduction"
        "Flow is a state in which an individual is completely immersed in
        an activity without reflective self-consciousness but with a deep sense of control.
        One goal of this study is figure out which physical activities are more or less conducive to flow.
        However, in the short term, we invite you to select characteristics that are of interest to you.
        Physical activities are ranked according to your criteria.
        Which physical activities that you haven’t tried yet might be worth considering?"
        h3
          className: "ui header"
          id: "data-explorer"
          "Data Explorer"
        "These results are obtained by fitting responses from participants with a statistical model.
        This model considers each characteristic independently of other characteristics.
        For example, "
        i({}, "creativity")
        " is ranked independently of "
        i({}, "novelty.")
        " Currently, sample size is limited and rankings may not match intuition very well.
        Model estimates should improve with additional data."
        br()
        br()
        div
          className: "ui stackable two column grid"
          weightPanel
            onClick: parent.handleChange
            abbreviateFacet: @state.abbreviateFacet
            setAbbreviateFacet: parent.setAbbreviateFacet
            weight: @state.weight
            resetWeights: parent.resetWeights
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
        "The graph below shows the responses from participants and how they fit together.
        Although #{RCPA_nodes.length} activities have been mentioned,
        we can only analyze those that are connected.
        World Sports Encyclopedia (2003) estimated that there are about eight thousand sports.
        Of course, physical activities are a superset of that.
        Even if we restrict our interest to, say, the 400 most popular physical activities,
        there are still about eighty thousand possible pairs.
        So far, data is available for #{RCPA_edges.length} pairs
        from #{RCPA_edges.map((v)->v.value).reduce((x,y)->x+y)} responses.
        Our statistical model can fill in the gaps, but the more data the better."
        React.createElement(Graph)
        h3
          className: "ui header"
          id: "invitation"
          "Invitation to Participate"
        "If you'd like to contribute more data,
        you can take this survey again with a different pair of physical activities."
        br()
        br()
        "If you enjoyed participating, you may want to invite others to participate. "
        """
        Suggested text for invitation: "I recently participated in a five minute
        survey that examined the relative characteristics of physical activities.
        I found it thought provoking and enjoyed going through it.
        You might enjoy it too. Take a look,
        """
        ' '
        a
          href: "http://tiny.cc/physical"
          "http://tiny.cc/physical"
        '"'
        h3
          className: "ui header"
          id: "contact"
          "Questions?"
        "Joshua Pritikin <"
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
