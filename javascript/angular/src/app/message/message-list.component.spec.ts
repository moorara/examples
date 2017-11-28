import { async, ComponentFixture, TestBed } from '@angular/core/testing'
import { RouterTestingModule } from '@angular/router/testing'
import { HttpClientModule } from '@angular/common/http'

import { MessageListComponent } from './message-list.component'
import { MessageService } from '../common/message.service'

describe('MessageListComponent', () => {
  let component: MessageListComponent
  let fixture: ComponentFixture<MessageListComponent>

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ RouterTestingModule, HttpClientModule ],
      providers: [ MessageService ],
      declarations: [ MessageListComponent ]
    })
    .compileComponents()
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MessageListComponent)
    component = fixture.componentInstance
    fixture.detectChanges()
  });

  it('should create', () => {
    expect(component).toBeTruthy()
  })
})
