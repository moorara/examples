import 'should'
import React from 'react'
import { shallow } from 'enzyme'
import ReactTestUtils from 'react-dom/test-utils'
import ShallowRenderer from 'react-test-renderer/shallow'

import CourseForm from './CourseForm'

describe('CourseFrom (Enzyme)', () => {
  let wrapper
  const setup = (isSaving) => {
    let props = {
      course: {},
      authors: [],
      isSaving,
      onChange: () => {},
      onSave: () => {},
      errors: {},
    }
    wrapper = shallow(<CourseForm {...props} />)
  }

  it('should render form and its elements', () => {
    setup()
    wrapper.find('form').length.should.equal(1)
    wrapper.find('TextInput').length.should.equal(3)
    wrapper.find('SelectInput').length.should.equal(1)
    wrapper.find('input').length.should.equal(1)
  })
  it('should label submit button "Save" when not saving', () => {
    setup(false)
    wrapper.find('input').props().value.should.equal('Save')
  })
  it('should label submit button "Saving ..." when not saving', () => {
    setup(true)
    wrapper.find('input').props().value.should.equal('Saving ...')
  })
})

describe('CourseForm (React Test Utils)', () => {
  let renderer, output
  const setup = (isSaving) => {
    let props = {
      course: {},
      authors: [],
      isSaving,
      onChange: () => {},
      onSave: () => {},
      errors: {},
    }
    renderer = new ShallowRenderer()
    renderer.render(<CourseForm {...props} />)
    output = renderer.getRenderOutput()
  }

  it('should render form and its elements', () => {
    setup()
    output.type.should.equal('form')
    ReactTestUtils.isElement(output.props.children[0]).should.be.true()
    ReactTestUtils.isElement(output.props.children[1]).should.be.true()
    ReactTestUtils.isElement(output.props.children[2]).should.be.true()
    ReactTestUtils.isElement(output.props.children[3]).should.be.true()
    ReactTestUtils.isElement(output.props.children[4]).should.be.true()
  })
  it('should label submit button "Save" when not saving', () => {
    setup(false)
    output.props.children[4].props.value.should.equal('Save')
  })
  it('should label submit button "Saving ..." when not saving', () => {
    setup(true)
    output.props.children[4].props.value.should.equal('Saving ...')
  })
})
