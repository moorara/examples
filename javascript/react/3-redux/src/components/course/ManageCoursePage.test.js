import React from 'react'
import { mount } from 'enzyme'
import Promise from 'bluebird'
import should from 'should'

import { ManageCoursePage, mapStateToProps, mapDispatchToProps } from './ManageCoursePage'

describe('ManageCoursePage (Enzyme)', () => {
  let wrapper

  const setup = () => {
    let props = {
      authors: [],
      actions: { saveCourse: () => Promise.resolve() },
      course: { id: '', title: '', url: '', authorId: '', length: '', category: '' }
    }
    wrapper = mount(<ManageCoursePage {...props} />)
  }

  it('mapStateToProps', () => {
    let state = {
      authors: [
        { id: '1111', firstName: 'Solomon', lastName: 'Hykes' },
        { id: '2222', firstName: 'Dan', lastName: 'Abramov' }
      ],
      courses: [
        { id: 'aaaa', title: 'react', url: 'https://reactjs.org', authorId: '1111', length: '1:00' },
        { id: 'bbbb', title: 'redux', url: 'http://redux.js.org', authorId: '2222', length: '1:00' }
      ]
    }
    let ownProps = {
      match: {
        params: {}
      }
    }
    let props = mapStateToProps(state, ownProps)
    props.course.should.eql({ id: '', title: '', url: '', authorId: '', length: '', category: '' })
    props.authors.should.eql([
      { value: '1111', text: 'Solomon Hykes' },
      { value: '2222', text: 'Dan Abramov' }
    ])
  })

  it('mapDispatchToProps', () => {
    let props = mapDispatchToProps()
    should.equal(typeof props.actions.saveCourse, 'function')
  })

  it('should set error when trying to save a course with empty title', () => {
    setup()
    let saveButton = wrapper.find('input').last()
    saveButton.prop('type').should.equal('submit')
    saveButton.simulate('click')
    wrapper.state().errors.title.should.equal('Title must be at least characters.')
  })
})
