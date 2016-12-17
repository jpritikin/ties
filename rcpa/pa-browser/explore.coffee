{ div, h1, p, a, form, td, tr, table, tbody, input, li, ul } = React.DOM

weightLine = (props) ->
    tr { key: props.key },
      td {}, props.facet
      td {},
        input
          type: "radio"
          name: props.facet
          value: 0
          defaultChecked: true
          onChange: () -> props.onClick(props.index, 0)
      td {},
        input
          type: "radio"
          name: props.facet
          value: 0.5
          onChange: () -> props.onClick(props.index, 0.5)
      td {},
        input
          type: "radio"
          name: props.facet
          value: 1.0
          onChange: () -> props.onClick(props.index, 1.0)

weightPanel = (props) ->
  lis = RCPA_FACETS.map((text, index) ->
    weightLine facet: text, key: text, index: index, onClick: props.onClick)
  div {},
    "Which characteristics do you value?"
    table {},
      tbody {},
        tr {},
          td {}
          td {}, "0.0"
          td {}, "0.5"
          td {}, "1.0"
        lis

dotprod = (v1, v2) ->
  throw new Error("dotprod: #{v1.length} != #{v2.length}") if v1.length isnt v2.length
  maxIndex = v1.length - 1
  out = 0
  out += v1[x] * v2[x] for x in [0..maxIndex] when (v1[x] isnt "NA" and v2[x] isnt "NA")
  out

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
  lis = order.map((index) ->
    li
      key: index
      a
        href: "https://en.wikipedia.org/wiki/#{encodeURI(RCPA_PA[index])}"
        target: "_blank"
        RCPA_PA[index])
  ul {},
    lis

class Jumbotron extends React.Component
  constructor: (props) ->
    super(props)
    @state =
      weight: new Array(RCPA_FACETS.length).fill(0)

  handleChange: (index, value) =>
    foo = @state.weight.slice()
    foo[index] = value
    this.setState
      weight: foo

  render: ->
    fn = this.handleChange
    div {},
      weightPanel
        onClick: (index, value) ->
          fn(index, value)
      resultPanel
        weight: @state.weight

ReactDOM.render(React.createElement(Jumbotron), document.getElementById('content'))
